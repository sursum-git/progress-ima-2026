&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

/* Local Variable Definitions ---                                       */
DEF INPUT-OUTPUT PARAMETER c-cod-estabel AS CHAR.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.
DEF INPUT-OUTPUT PARAMETER c-dt-limite        AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-it-codigo-ini    AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-it-codigo-fin    AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-refer-ini    AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-refer-fin    AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-qualid-ini   AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-qualid-fin   AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-obsoleto-ini AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-obsoleto-fin AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-corte-comerc-ini AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-corte-comerc-fin AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-emitente-ini AS CHAR.                            
DEF INPUT-OUTPUT PARAMETER c-cod-emitente-fin AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-depos        AS CHAR.
DEF INPUT-OUTPUT PARAMETER l-lote-todos       AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-pp          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-pd          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-rp          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-rd          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-sc          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-ca          AS LOG.
DEF INPUT-OUTPUT PARAMETER c-tp-artigo        AS CHAR.
DEF INPUT-OUTPUT PARAMETER i-opc-acabado      AS INT.
DEF INPUT-OUTPUT PARAMETER l-itens-relac      AS LOG.
DEF INPUT-OUTPUT PARAMETER l-dep-corte        AS LOG.
DEF INPUT-OUTPUT PARAMETER l-ok               AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cod-estabel fi-dt-limite fi-it-codigo-ini ~
fi-it-codigo-fin bt-dig-item fi-cod-refer-ini bt-ex-item fi-cod-refer-fin ~
fi-cod-qualid-ini bt-dig-ref fi-cod-qualid-fin fi-cod-obsoleto-ini ~
bt-ex-ref fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin ~
bt-dig-qualid fi-nome-abrev-ini bt-ex-qualid fi-nome-abrev-fin fi-cod-depos ~
tg-dep-corte bt-dig-cob bt-ex-cob tg-lote-todos bt-dig-ccom bt-ex-ccom ~
rs-opc-artigo bt-dig-cli bt-ex-cli tg-itens-relac bt-ok bt-cancelar ~
bt-ajuda rs-opc-acab IMAGE-2 IMAGE-51 IMAGE-52 IMAGE-73 IMAGE-75 IMAGE-77 ~
IMAGE-78 IMAGE-79 IMAGE-82 IMAGE-83 IMAGE-84 IMAGE-87 RECT-1 RECT-49 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-nome-estabel ~
fi-dt-limite fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-cod-qualid-ini fi-cod-qualid-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin ~
fi-nome-abrev-ini fi-nome-abrev-fin fi-cod-depos tg-dep-corte tg-lote-todos ~
tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca ~
rs-opc-artigo tg-itens-relac rs-opc-acab 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-dig-ccom 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Corte Comercial".

DEFINE BUTTON bt-dig-cli 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Cliente".

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

DEFINE BUTTON bt-dig-qualid 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Qualidade".

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

DEFINE BUTTON bt-ex-cli 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Cliente".

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

DEFINE BUTTON bt-ex-qualid 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Qualidade".

DEFINE BUTTON bt-ex-ref 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Referˆncias".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-cod-depos AS CHARACTER FORMAT "x(3)" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Codigo Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-qualid-fin AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo de qualidade final." NO-UNDO.

DEFINE VARIABLE fi-cod-qualid-ini AS CHARACTER FORMAT "X(1)" 
     LABEL "Qualidade" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo de qualidade inicial." NO-UNDO.

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

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" 
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

DEFINE VARIABLE fi-nome-abrev-fin AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R17 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-75
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-77
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-78
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-79
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-82
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-83
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-84
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-87
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE VARIABLE rs-opc-acab AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Liso", 1,
"Estampado", 2,
"Ambos", 3
     SIZE 41.14 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 41.14 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 73 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 14.25.

DEFINE VARIABLE tg-dep-corte AS LOGICAL INITIAL no 
     LABEL "Considerar Rolos do Dep¢sito Cortes de Amostra" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-itens-relac AS LOGICAL INITIAL no 
     LABEL "Verificar Itens Relacionados" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.86 BY .83 NO-UNDO.

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

DEFINE VARIABLE tg-lote-todos AS LOGICAL INITIAL yes 
     LABEL "TODOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-estabel AT ROW 2.04 COL 15 COLON-ALIGNED WIDGET-ID 2
     fi-nome-estabel AT ROW 2.04 COL 18.43 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-dt-limite AT ROW 3.04 COL 15 COLON-ALIGNED
     fi-it-codigo-ini AT ROW 4.04 COL 15 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 4.04 COL 45 COLON-ALIGNED NO-LABEL
     bt-dig-item AT ROW 4 COL 65 NO-TAB-STOP 
     fi-cod-refer-ini AT ROW 5.04 COL 15 COLON-ALIGNED
     bt-ex-item AT ROW 4 COL 70 NO-TAB-STOP 
     fi-cod-refer-fin AT ROW 5.04 COL 45 COLON-ALIGNED NO-LABEL
     fi-cod-qualid-ini AT ROW 6.04 COL 15 COLON-ALIGNED
     bt-dig-ref AT ROW 5 COL 65 NO-TAB-STOP 
     fi-cod-qualid-fin AT ROW 6.04 COL 45 COLON-ALIGNED NO-LABEL
     fi-cod-obsoleto-ini AT ROW 7.04 COL 15 COLON-ALIGNED HELP
          "Codigo obsoleto"
     bt-ex-ref AT ROW 5 COL 70 NO-TAB-STOP 
     fi-cod-obsoleto-fin AT ROW 7.04 COL 45 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-corte-comerc-ini AT ROW 8.04 COL 15 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 8.04 COL 45 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     bt-dig-qualid AT ROW 6 COL 65 NO-TAB-STOP 
     fi-nome-abrev-ini AT ROW 9.04 COL 15 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor"
     bt-ex-qualid AT ROW 6 COL 70 NO-TAB-STOP 
     fi-nome-abrev-fin AT ROW 9.04 COL 45 COLON-ALIGNED HELP
          "Nome abreviado do cliente/fornecedor" NO-LABEL
     fi-cod-depos AT ROW 10.04 COL 15 COLON-ALIGNED HELP
          "C¢digo do Dep¢sito"
     tg-dep-corte AT ROW 11.04 COL 17
     bt-dig-cob AT ROW 7 COL 65 NO-TAB-STOP 
     bt-ex-cob AT ROW 7 COL 70 NO-TAB-STOP 
     tg-lote-todos AT ROW 12.71 COL 17
     tg-lote-pp AT ROW 12.71 COL 30.72
     tg-lote-pd AT ROW 12.71 COL 37
     tg-lote-rp AT ROW 12.71 COL 43.14
     bt-dig-ccom AT ROW 8 COL 65 NO-TAB-STOP 
     tg-lote-rd AT ROW 12.71 COL 49.14
     bt-ex-ccom AT ROW 8 COL 70 NO-TAB-STOP 
     tg-lote-sc AT ROW 12.71 COL 55.14
     tg-lote-ca AT ROW 12.71 COL 61.14
     rs-opc-artigo AT ROW 13.63 COL 16.86 NO-LABEL
     bt-dig-cli AT ROW 9 COL 65 NO-TAB-STOP 
     bt-ex-cli AT ROW 9 COL 70 NO-TAB-STOP 
     tg-itens-relac AT ROW 11.83 COL 17
     bt-ok AT ROW 15.88 COL 3
     bt-cancelar AT ROW 15.88 COL 13.57
     bt-ajuda AT ROW 15.88 COL 64.29
     rs-opc-acab AT ROW 14.5 COL 16.86 NO-LABEL WIDGET-ID 8
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 12.83 COL 13
     "Acabamento:" VIEW-AS TEXT
          SIZE 9.43 BY .54 AT ROW 14.58 COL 7.57 WIDGET-ID 6
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 13.46 COL 6.43
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     IMAGE-2 AT ROW 4.04 COL 43
     IMAGE-51 AT ROW 5.04 COL 34
     IMAGE-52 AT ROW 5.04 COL 43
     IMAGE-73 AT ROW 4.04 COL 34
     IMAGE-75 AT ROW 6.04 COL 34
     IMAGE-77 AT ROW 7.04 COL 34
     IMAGE-78 AT ROW 8.04 COL 34
     IMAGE-79 AT ROW 9.04 COL 34
     IMAGE-82 AT ROW 6.04 COL 43
     IMAGE-83 AT ROW 7.04 COL 43
     IMAGE-84 AT ROW 9.04 COL 43
     IMAGE-87 AT ROW 8.04 COL 43
     RECT-1 AT ROW 15.63 COL 2
     RECT-49 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.14 BY 16.08
         FONT 1
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Parƒmetros e Sele‡Æo da Analise Gerencial - ESSP0150A"
         HEIGHT             = 16.13
         WIDTH              = 75
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-ca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-pd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-pp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-sc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Parƒmetros e Sele‡Æo da Analise Gerencial - ESSP0150A */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Parƒmetros e Sele‡Æo da Analise Gerencial - ESSP0150A */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  ASSIGN l-ok = NO.
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-ccom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ccom w-window
ON CHOOSE OF bt-dig-ccom IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Corte_Comercial").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-cli w-window
ON CHOOSE OF bt-dig-cli IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Cliente").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-cob w-window
ON CHOOSE OF bt-dig-cob IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Codigo_Obsoleto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-item w-window
ON CHOOSE OF bt-dig-item IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-qualid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-qualid w-window
ON CHOOSE OF bt-dig-qualid IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Qualidade").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ref w-window
ON CHOOSE OF bt-dig-ref IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Referˆncia").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ccom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ccom w-window
ON CHOOSE OF bt-ex-ccom IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Corte_Comercial").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-cli w-window
ON CHOOSE OF bt-ex-cli IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Cliente").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-cob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-cob w-window
ON CHOOSE OF bt-ex-cob IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita,
                              INPUT "E",
                              INPUT "Codigo_Obsoleto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-item w-window
ON CHOOSE OF bt-ex-item IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Item").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-qualid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-qualid w-window
ON CHOOSE OF bt-ex-qualid IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Qualidade").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ref w-window
ON CHOOSE OF bt-ex-ref IN FRAME F-Main
DO:
   RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                             INPUT "E",
                             INPUT "Referˆncia").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-lote-pp = 'NO' AND
     INPUT FRAME {&FRAME-NAME} tg-lote-pd = 'NO' AND
     INPUT FRAME {&FRAME-NAME} tg-lote-rp = 'NO' AND
     INPUT FRAME {&FRAME-NAME} tg-lote-rd = 'NO' AND
     INPUT FRAME {&FRAME-NAME} tg-lote-sc = 'NO' AND
     INPUT FRAME {&FRAME-NAME} tg-lote-ca = 'NO' THEN
     ASSIGN tg-lote-todos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.

  ASSIGN c-cod-estabel      = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
         c-dt-limite        = INPUT FRAME {&FRAME-NAME} fi-dt-limite
         c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
         c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin
         c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
         c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
         c-cod-qualid-ini   = INPUT FRAME {&FRAME-NAME} fi-cod-qualid-ini
         c-cod-qualid-fin   = INPUT FRAME {&FRAME-NAME} fi-cod-qualid-fin
         c-cod-obsoleto-ini = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini
         c-cod-obsoleto-fin = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
         c-corte-comerc-ini = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
         c-corte-comerc-fin = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
         c-cod-emitente-ini = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
         c-cod-emitente-fin = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin
         c-cod-depos        = INPUT FRAME {&FRAME-NAME} fi-cod-depos
         l-lote-todos       = INPUT FRAME {&FRAME-NAME} tg-lote-todos
         l-lote-pp          = INPUT FRAME {&FRAME-NAME} tg-lote-pp
         l-lote-pd          = INPUT FRAME {&FRAME-NAME} tg-lote-pd
         l-lote-rp          = INPUT FRAME {&FRAME-NAME} tg-lote-rp
         l-lote-rd          = INPUT FRAME {&FRAME-NAME} tg-lote-rd
         l-lote-sc          = INPUT FRAME {&FRAME-NAME} tg-lote-sc
         l-lote-ca          = INPUT FRAME {&FRAME-NAME} tg-lote-ca
         c-tp-artigo        = INPUT FRAME {&FRAME-NAME} rs-opc-artigo
         i-opc-acabado      = INPUT FRAME {&FRAME-NAME} rs-opc-acab
         l-itens-relac      = INPUT FRAME {&FRAME-NAME} tg-itens-relac 
         l-dep-corte        = INPUT FRAME {&FRAME-NAME} tg-dep-corte
         l-ok               = YES.

  APPLY "close":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-window
ON LEAVE OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento nÆo Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini w-window
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME F-Main /* Codigo Obsoleto */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-qualid-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-qualid-ini w-window
ON LEAVE OF fi-cod-qualid-ini IN FRAME F-Main /* Qualidade */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-cod-qualid-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-fin
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-window
ON LEAVE OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-fin
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini w-window
ON LEAVE OF fi-corte-comerc-ini IN FRAME F-Main /* Corte Comercial */
DO:
  IF SELF:SCREEN-VALUE = '' THEN DO.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-ini IN FRAME F-Main /* Corte Comercial */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-ini
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite w-window
ON LEAVE OF fi-dt-limite IN FRAME F-Main /* Data Limite */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                       &campo     = fi-it-codigo-fin
                       &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-window
ON LEAVE OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-window
ON LEAVE OF fi-nome-abrev-fin IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE <> '' THEN DO:
       FIND emitente WHERE 
            emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF NOT AVAIL emitente THEN
          FIND emitente WHERE 
               STRING(emitente.cod-emit) = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL emitente THEN
           ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
       ELSE
           ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ZZZZZZZZZZZZ".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-fin
                       &campozoom = nome-abrev}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-window
ON LEAVE OF fi-nome-abrev-ini IN FRAME F-Main /* Cliente */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO:
      FIND emitente WHERE 
           emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN
         FIND emitente WHERE 
              STRING(emitente.cod-emit) = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN
          ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev
                 fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
      ELSE
          ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-ini IN FRAME F-Main /* Cliente */
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-ini
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-lote-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-lote-todos w-window
ON VALUE-CHANGED OF tg-lote-todos IN FRAME F-Main /* TODOS */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-pp)
            tg-lote-pd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-pd)
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-rp)
            tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-rd).
  ELSE
      ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
             tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
             l-lote-pp = YES
             l-lote-rp = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */
fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-corte-comerc-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-corte-comerc-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-nome-abrev-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-nome-abrev-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY fi-cod-estabel fi-nome-estabel fi-dt-limite fi-it-codigo-ini 
          fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-cod-qualid-ini 
          fi-cod-qualid-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin 
          fi-corte-comerc-ini fi-corte-comerc-fin fi-nome-abrev-ini 
          fi-nome-abrev-fin fi-cod-depos tg-dep-corte tg-lote-todos tg-lote-pp 
          tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo 
          tg-itens-relac rs-opc-acab 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE fi-cod-estabel fi-dt-limite fi-it-codigo-ini fi-it-codigo-fin 
         bt-dig-item fi-cod-refer-ini bt-ex-item fi-cod-refer-fin 
         fi-cod-qualid-ini bt-dig-ref fi-cod-qualid-fin fi-cod-obsoleto-ini 
         bt-ex-ref fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin 
         bt-dig-qualid fi-nome-abrev-ini bt-ex-qualid fi-nome-abrev-fin 
         fi-cod-depos tg-dep-corte bt-dig-cob bt-ex-cob tg-lote-todos 
         bt-dig-ccom bt-ex-ccom rs-opc-artigo bt-dig-cli bt-ex-cli 
         tg-itens-relac bt-ok bt-cancelar bt-ajuda rs-opc-acab IMAGE-2 IMAGE-51 
         IMAGE-52 IMAGE-73 IMAGE-75 IMAGE-77 IMAGE-78 IMAGE-79 IMAGE-82 
         IMAGE-83 IMAGE-84 IMAGE-87 RECT-1 RECT-49 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
/*  {utp/ut9000.i "XX9999" "9.99.99.999"} */

  ASSIGN fi-cod-estabel      = c-cod-estabel
         fi-dt-limite        = c-dt-limite
         fi-it-codigo-ini    = c-it-codigo-ini 
         fi-it-codigo-fin    = c-it-codigo-fin 
         fi-cod-refer-ini    = c-cod-refer-ini 
         fi-cod-refer-fin    = c-cod-refer-fin
         fi-cod-qualid-ini   = c-cod-qualid-ini      
         fi-cod-qualid-fin   = c-cod-qualid-fin      
         fi-cod-obsoleto-ini = c-cod-obsoleto-ini 
         fi-cod-obsoleto-fin = c-cod-obsoleto-fin 
         fi-corte-comerc-ini = c-corte-comerc-ini 
         fi-corte-comerc-fin = c-corte-comerc-fin 
         fi-nome-abrev-ini   = c-cod-emitente-ini 
         fi-nome-abrev-fin   = c-cod-emitente-fin 
         fi-cod-depos        = c-cod-depos
         tg-lote-todos       = l-lote-todos
         tg-itens-relac      = l-itens-relac
         rs-opc-artigo       = c-tp-artigo
         rs-opc-acab         = i-opc-acabado.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF NOT tg-lote-todos THEN DO:
     APPLY 'value-changed' TO tg-lote-todos IN FRAME {&FRAME-NAME}.

     ASSIGN tg-lote-pp = l-lote-pp
            tg-lote-pd = l-lote-pd
            tg-lote-rp = l-lote-rp
            tg-lote-rd = l-lote-rd
            tg-lote-sc = l-lote-sc
            tg-lote-ca = l-lote-ca.

  END.
 
   /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'TAB' TO fi-cod-estabel. 



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

