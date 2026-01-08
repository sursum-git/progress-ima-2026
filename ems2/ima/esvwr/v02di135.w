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

/* Parameters Definitions ---                                           */
DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.
DEF VAR c-dia AS CHAR.
DEF VAR c-classes AS CHAR.

DEF NEW GLOBAL SHARED VAR h-essp0164 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b02di135 AS HANDLE NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-100 IMAGE-101 IMAGE-108 IMAGE-109 ~
IMAGE-110 IMAGE-112 IMAGE-113 IMAGE-114 IMAGE-116 IMAGE-117 rt-key ~
IMAGE-118 IMAGE-119 RECT-61 fi-cod-estab-ini fi-cod-estab-fin bt-dig-est ~
bt-ex-est fi-dt-periodo-ini fi-dt-periodo-fin fi-no-ab-reppri-ini ~
fi-no-ab-reppri-fin bt-dig-rep bt-ex-rep fi-nr-nota-fis-ini ~
fi-nr-nota-fis-fin bt-dig-nf bt-ex-nf fi-it-codigo-ini fi-it-codigo-fin ~
bt-dig-item bt-ex-item fi-cond-pagto-ini fi-cond-pagto-fin bt-dig-cond ~
bt-ex-cond tg-ger-g tg-pracista tg-ger-l tg-interno tg-externo bt-confirma 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estab-ini fi-cod-estab-fin ~
fi-dt-periodo-ini fi-dt-periodo-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin ~
fi-nr-nota-fis-ini fi-nr-nota-fis-fin fi-it-codigo-ini fi-it-codigo-fin ~
fi-cond-pagto-ini fi-cond-pagto-fin tg-ger-g tg-pracista tg-ger-l ~
tg-interno tg-externo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-cod-estab-ini fi-cod-estab-fin fi-no-ab-reppri-ini ~
fi-no-ab-reppri-fin fi-it-codigo-ini fi-it-codigo-fin 
&Scoped-define List-5 fi-no-ab-reppri-ini fi-no-ab-reppri-fin 

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
DEFINE BUTTON bt-confirma 
     LABEL "Processa o C lculo de Comissäes" 
     SIZE 46.43 BY 1.58.

DEFINE BUTTON bt-dig-cond 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-est 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-item 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-nf 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Representante".

DEFINE BUTTON bt-dig-rep 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Representante".

DEFINE BUTTON bt-ex-cond 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-est 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-item 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-nf 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Representante".

DEFINE BUTTON bt-ex-rep 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Representante".

DEFINE VARIABLE fi-cod-estab-fin AS CHARACTER FORMAT "X(4)":U INITIAL "5" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estab-ini AS CHARACTER FORMAT "X(4)":U INITIAL "1" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cond-pagto-fin AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cond-pagto-ini AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Condi‡Æo de Pagamento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-periodo-fin AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Periodo Final" NO-UNDO.

DEFINE VARIABLE fi-dt-periodo-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Periodo Inicial" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Produto" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-fin AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Representante Final" NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Representante":R28 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Representante Inicial" NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Nr Nota Fiscal":R17 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-108
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-110
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-112
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-113
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-114
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-116
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-117
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-118
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-119
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 4.5.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 19.5.

DEFINE VARIABLE tg-externo AS LOGICAL INITIAL no 
     LABEL "Externo" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-ger-g AS LOGICAL INITIAL no 
     LABEL "Gerente Geral" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-ger-l AS LOGICAL INITIAL no 
     LABEL "Gerente Loja" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-interno AS LOGICAL INITIAL no 
     LABEL "Interno" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pracista AS LOGICAL INITIAL no 
     LABEL "Pracista" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-cod-estab-ini AT ROW 3.75 COL 30 COLON-ALIGNED WIDGET-ID 46
     fi-cod-estab-fin AT ROW 3.75 COL 69.57 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     bt-dig-est AT ROW 3.75 COL 89.57 WIDGET-ID 40 NO-TAB-STOP 
     bt-ex-est AT ROW 3.75 COL 94.57 WIDGET-ID 42 NO-TAB-STOP 
     fi-dt-periodo-ini AT ROW 4.75 COL 30 COLON-ALIGNED
     fi-dt-periodo-fin AT ROW 4.79 COL 69.72 COLON-ALIGNED NO-LABEL
     fi-no-ab-reppri-ini AT ROW 5.75 COL 30.14 COLON-ALIGNED HELP
          "Nome abreviado do representante principal"
     fi-no-ab-reppri-fin AT ROW 5.75 COL 69.72 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     bt-dig-rep AT ROW 5.75 COL 89.72 WIDGET-ID 14 NO-TAB-STOP 
     bt-ex-rep AT ROW 5.75 COL 94.72 WIDGET-ID 16 NO-TAB-STOP 
     fi-nr-nota-fis-ini AT ROW 6.75 COL 30.14 COLON-ALIGNED HELP
          "N£mero da nota fiscal" WIDGET-ID 6
     fi-nr-nota-fis-fin AT ROW 6.75 COL 69.72 COLON-ALIGNED HELP
          "N£mero da nota fiscal" NO-LABEL WIDGET-ID 8
     bt-dig-nf AT ROW 6.75 COL 89.72 WIDGET-ID 26 NO-TAB-STOP 
     bt-ex-nf AT ROW 6.75 COL 94.72 WIDGET-ID 28 NO-TAB-STOP 
     fi-it-codigo-ini AT ROW 7.75 COL 30.14 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 7.75 COL 69.72 COLON-ALIGNED NO-LABEL
     bt-dig-item AT ROW 7.75 COL 89.72 WIDGET-ID 18 NO-TAB-STOP 
     bt-ex-item AT ROW 7.75 COL 94.72 WIDGET-ID 20 NO-TAB-STOP 
     fi-cond-pagto-ini AT ROW 8.75 COL 30.14 COLON-ALIGNED WIDGET-ID 10
     fi-cond-pagto-fin AT ROW 8.75 COL 69.72 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     bt-dig-cond AT ROW 8.75 COL 89.72 WIDGET-ID 30 NO-TAB-STOP 
     bt-ex-cond AT ROW 8.75 COL 94.72 WIDGET-ID 32 NO-TAB-STOP 
     tg-ger-g AT ROW 11.5 COL 36 WIDGET-ID 62
     tg-pracista AT ROW 11.5 COL 56 WIDGET-ID 66
     tg-ger-l AT ROW 12.5 COL 36 WIDGET-ID 64
     tg-interno AT ROW 12.5 COL 56 WIDGET-ID 68
     tg-externo AT ROW 13.46 COL 56 WIDGET-ID 70
     bt-confirma AT ROW 16.5 COL 32
     " Classe do Representante" VIEW-AS TEXT
          SIZE 18 BY .75 AT ROW 10.13 COL 34 WIDGET-ID 60
     IMAGE-100 AT ROW 5.75 COL 67.57
     IMAGE-101 AT ROW 5.75 COL 49.14
     IMAGE-108 AT ROW 8.75 COL 49.14
     IMAGE-109 AT ROW 7.75 COL 49.14
     IMAGE-110 AT ROW 6.75 COL 49.14
     IMAGE-112 AT ROW 8.75 COL 67.57
     IMAGE-113 AT ROW 7.75 COL 67.57
     IMAGE-114 AT ROW 6.75 COL 67.57
     IMAGE-116 AT ROW 4.75 COL 49.14
     IMAGE-117 AT ROW 4.75 COL 67.57
     rt-key AT ROW 1 COL 1
     IMAGE-118 AT ROW 3.75 COL 49 WIDGET-ID 48
     IMAGE-119 AT ROW 3.75 COL 67.43 WIDGET-ID 50
     RECT-61 AT ROW 10.5 COL 32 WIDGET-ID 72
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
         HEIGHT             = 19.67
         WIDTH              = 118.72.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-estab-fin IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-estab-ini IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME f-main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-no-ab-reppri-fin IN FRAME f-main
   4 5                                                                  */
ASSIGN 
       fi-no-ab-reppri-fin:PRIVATE-DATA IN FRAME f-main     = 
                "ZZZZZZZZZZZZ".

/* SETTINGS FOR FILL-IN fi-no-ab-reppri-ini IN FRAME f-main
   4 5                                                                  */
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

&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma V-table-Win
ON CHOOSE OF bt-confirma IN FRAME f-main /* Processa o C lculo de Comissäes */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-estab-ini    
         INPUT FRAME {&FRAME-NAME} fi-cod-estab-fin
         INPUT FRAME {&FRAME-NAME} fi-dt-periodo-ini    
         INPUT FRAME {&FRAME-NAME} fi-dt-periodo-fin
         INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
         INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
         INPUT FRAME {&FRAME-NAME} fi-nr-nota-fis-ini
         INPUT FRAME {&FRAME-NAME} fi-nr-nota-fis-fin
         INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
         INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin    
         INPUT FRAME {&FRAME-NAME} fi-cond-pagto-ini
         INPUT FRAME {&FRAME-NAME} fi-cond-pagto-fin.
    
  ASSIGN INPUT FRAME {&FRAME-NAME} tg-ger-l tg-ger-g tg-pracista tg-interno tg-externo.

  IF tg-ger-l = NO AND 
     tg-ger-g = NO AND 
     tg-interno = NO AND
     tg-externo = NO AND 
     tg-pracista = NO THEN DO.

     MESSAGE 'Favor Escolher pelo menos uma Classe'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO tg-ger-g.
     RETURN NO-APPLY.
  END.

  ASSIGN c-classes = ''.
  IF tg-ger-g THEN
     ASSIGN c-classes = '1'.

  IF tg-ger-l THEN
     ASSIGN c-classes = IF c-classes = ''
                        THEN '2'
                        ELSE c-classes + ',2'.

  IF tg-pracista THEN
     ASSIGN c-classes = IF c-classes = ''
                        THEN '3'
                        ELSE c-classes + ',3'.

  IF tg-interno THEN
     ASSIGN c-classes = IF c-classes = ''
                        THEN '4'
                        ELSE c-classes + ',4'.
  IF tg-externo THEN
     ASSIGN c-classes = IF c-classes = ''
                        THEN '5'
                        ELSE c-classes + ',5'.

  RUN pi-select-page IN h-essp0164 (INPUT 2).

  RUN pi-processa IN h-b02di135 (INPUT TABLE tt-digita,
                                 INPUT fi-cod-estab-ini,
                                 INPUT fi-cod-estab-fin,
                                 INPUT fi-dt-periodo-ini,   
                                 INPUT fi-dt-periodo-fin,   
                                 INPUT fi-no-ab-reppri-ini,    
                                 INPUT fi-no-ab-reppri-fin,    
                                 INPUT fi-nr-nota-fis-ini,    
                                 INPUT fi-nr-nota-fis-fin,    
                                 INPUT fi-it-codigo-ini,   
                                 INPUT fi-it-codigo-fin,   
                                 INPUT fi-cond-pagto-ini, 
                                 INPUT fi-cond-pagto-fin,
                                 INPUT c-classes).

  FOR EACH tt-digita.
      DELETE tt-digita.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-cond
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-cond V-table-Win
ON CHOOSE OF bt-dig-cond IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Condi‡Æo Pagamento").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-est V-table-Win
ON CHOOSE OF bt-dig-est IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Estab").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-item V-table-Win
ON CHOOSE OF bt-dig-item IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Produto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-nf V-table-Win
ON CHOOSE OF bt-dig-nf IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "N§ Nota Fiscal").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-rep V-table-Win
ON CHOOSE OF bt-dig-rep IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Representante").

    FOR EACH tt-digita WHERE
             tt-digita.campo = 'Representante'.

        FIND repres WHERE
             repres.cod-rep = INT(tt-digita.valor) NO-LOCK NO-ERROR.
        IF NOT AVAIL repres THEN
           FIND repres WHERE
                repres.nome-abrev = tt-digita.valor NO-LOCK NO-ERROR.

        IF NOT AVAIL repres THEN DO.
           MESSAGE "Representante " tt-digita.valor " nƒo Cadastrado..."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ASSIGN tt-digita.valor = repres.nome-abrev.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-cond
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-cond V-table-Win
ON CHOOSE OF bt-ex-cond IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Condi‡Æo Pagamento").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-est V-table-Win
ON CHOOSE OF bt-ex-est IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Estab").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-item V-table-Win
ON CHOOSE OF bt-ex-item IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Produto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-nf V-table-Win
ON CHOOSE OF bt-ex-nf IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "N§ Nota Fiscal").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-rep V-table-Win
ON CHOOSE OF bt-ex-rep IN FRAME f-main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Representante").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estab-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estab-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estab-ini IN FRAME f-main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
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
ON LEAVE OF fi-it-codigo-ini IN FRAME f-main /* Produto */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME f-main /* Produto */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-fin V-table-Win
ON LEAVE OF fi-no-ab-reppri-fin IN FRAME f-main
DO:
    IF SELF:SCREEN-VALUE <> "ZZZZZZZZZZZZ" THEN DO.
       FIND repres WHERE
            repres.cod-rep = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
       IF NOT AVAIL repres THEN
          FIND repres WHERE
               repres.nome-abrev = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
    
       IF NOT AVAIL repres THEN DO.
          MESSAGE "Representante nƒo Cadastrado..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

          APPLY "entry" TO SELF.
          RETURN NO-APPLY.
       END.

       ASSIGN SELF:SCREEN-VALUE = repres.nome-abrev.

       ASSIGN tg-pracista:SENSITIVE = NO
              tg-interno:SENSITIVE = NO
              tg-externo:SENSITIVE = NO
              tg-ger-g:SENSITIVE = NO
              tg-ger-l:SENSITIVE = NO. 
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-fin IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-fin
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini V-table-Win
ON LEAVE OF fi-no-ab-reppri-ini IN FRAME f-main /* Representante */
DO:
    ASSIGN tg-pracista:SENSITIVE = YES
           tg-interno:SENSITIVE = YES
           tg-externo:SENSITIVE = YES
           tg-ger-g:SENSITIVE = YES
           tg-ger-l:SENSITIVE = YES. 

    ASSIGN tg-ger-l:SCREEN-VALUE = 'NO'
           tg-ger-g:SCREEN-VALUE = 'NO'
           tg-pracista:SCREEN-VALUE = 'NO'
           tg-interno:SCREEN-VALUE = 'NO'
           tg-externo:SCREEN-VALUE = 'NO'.

   IF SELF:SCREEN-VALUE <> "" THEN DO.
      FIND repres WHERE
           repres.cod-rep = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAIL repres THEN
         FIND repres WHERE
              repres.nome-abrev = SELF:INPUT-VALUE NO-LOCK NO-ERROR.

      IF NOT AVAIL repres THEN DO.
         MESSAGE "Representante nƒo Cadastrado..."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO SELF.
         RETURN NO-APPLY.
      END.

      ASSIGN SELF:SCREEN-VALUE = repres.nome-abrev
             fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev.

      ASSIGN tg-pracista:SENSITIVE = NO
             tg-interno:SENSITIVE = NO
             tg-externo:SENSITIVE = NO
             tg-ger-g:SENSITIVE = NO
             tg-ger-l:SENSITIVE = NO. 

      ASSIGN tg-ger-l:SCREEN-VALUE = 'YES'
             tg-ger-g:SCREEN-VALUE = 'YES'
             tg-pracista:SCREEN-VALUE = 'YES'
             tg-interno:SCREEN-VALUE = 'YES'
             tg-externo:SCREEN-VALUE = 'YES'.
   END.
   ELSE
       fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-no-ab-reppri-fin:PRIVATE-DATA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-ini IN FRAME f-main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-ini
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-externo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-externo V-table-Win
ON VALUE-CHANGED OF tg-externo IN FRAME f-main /* Externo */
DO:
    RUN pi-trata-classe (INPUT 'O').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-ger-g
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-ger-g V-table-Win
ON VALUE-CHANGED OF tg-ger-g IN FRAME f-main /* Gerente Geral */
DO:
    RUN pi-trata-classe (INPUT 'G').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-ger-l
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-ger-l V-table-Win
ON VALUE-CHANGED OF tg-ger-l IN FRAME f-main /* Gerente Loja */
DO:
    RUN pi-trata-classe (INPUT 'G').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-interno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-interno V-table-Win
ON VALUE-CHANGED OF tg-interno IN FRAME f-main /* Interno */
DO:
    RUN pi-trata-classe (INPUT 'O').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-pracista
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-pracista V-table-Win
ON VALUE-CHANGED OF tg-pracista IN FRAME f-main /* Pracista */
DO:
    RUN pi-trata-classe (INPUT 'O').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-no-ab-reppri-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-no-ab-reppri-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY 'entry' TO fi-cod-estab-ini IN FRAME {&FRAME-NAME}.

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
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

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
  FOR EACH tt-digita.
      DELETE tt-digita.
  END.

                         
  ASSIGN fi-cod-estab-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '1'
         fi-cod-estab-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '5'
         fi-dt-periodo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(TODAY - DAY(TODAY) + 1)
         fi-dt-periodo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(TODAY)
         fi-no-ab-reppri-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
         fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'ZZZZZZZZZZZZ'
         fi-nr-nota-fis-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ''
         fi-nr-nota-fis-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = 'ZZZZZZZZZZZZZZZZ'
         fi-it-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-it-codigo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZZZZZZZZZ'
         fi-cond-pagto-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ''
         fi-cond-pagto-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = '999'.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-classe V-table-Win 
PROCEDURE pi-trata-classe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-classe AS CHAR.

    ASSIGN tg-pracista:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           tg-interno:SENSITIVE = YES
           tg-externo:SENSITIVE = YES
           tg-ger-g:SENSITIVE = YES
           tg-ger-l:SENSITIVE = YES.

    IF p-classe = 'G' THEN DO.
        ASSIGN tg-ger-l:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               tg-ger-g:SENSITIVE = YES.
        IF INPUT FRAME {&FRAME-NAME} tg-ger-l = YES OR
           INPUT FRAME {&FRAME-NAME} tg-ger-g = YES THEN DO.
           ASSIGN tg-pracista:SCREEN-VALUE = 'NO'
                  tg-interno:SCREEN-VALUE = 'NO'
                  tg-externo:SCREEN-VALUE = 'NO'.
    
           ASSIGN tg-pracista:SENSITIVE = NO
                  tg-interno:SENSITIVE = NO
                  tg-externo:SENSITIVE = NO.
        END.
    END.
    ELSE DO.
        ASSIGN tg-interno:SENSITIVE = YES
               tg-externo:SENSITIVE = YES
               tg-pracista:SENSITIVE = YES.
        
        IF INPUT FRAME {&FRAME-NAME} tg-interno = YES OR
           INPUT FRAME {&FRAME-NAME} tg-externo = YES OR
           INPUT FRAME {&FRAME-NAME} tg-pracista = YES THEN DO.
           ASSIGN tg-ger-g:SCREEN-VALUE = 'NO'
                  tg-ger-l:SCREEN-VALUE = 'NO'.
        
           ASSIGN tg-ger-g:SENSITIVE = NO.
                  tg-ger-l:SENSITIVE = NO.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

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

