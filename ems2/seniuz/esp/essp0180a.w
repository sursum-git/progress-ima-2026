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
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.

/* Local Variable Definitions ---                                       */
DEFINE INPUT-OUTPUT PARAMETER da-dt-entrega-fin   AS DATE.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ped-item-ext.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ped-item-ext.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-matriz-ini        LIKE emitente.nome-matriz.
DEFINE INPUT-OUTPUT PARAMETER c-matriz-fin        LIKE emitente.nome-matriz.
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-fin    LIKE ped-venda.nome-abrev.
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri.
DEFINE INPUT-OUTPUT PARAMETER c-opc-artigo        AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-100 IMAGE-101 IMAGE-3 IMAGE-4 IMAGE-5 ~
IMAGE-6 IMAGE-90 IMAGE-91 IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 ~
RECT-50 fi-dt-entrega-fin fi-nr-pedcli-ini fi-nr-pedcli-fin bt-dig-ped ~
bt-ex-ped fi-it-codigo-ini fi-it-codigo-fin bt-dig-item bt-ex-item ~
fi-cod-refer-ini fi-cod-refer-fin bt-dig-ref bt-ex-ref fi-matriz-ini ~
fi-matriz-fin bt-dig-mat bt-ex-mat fi-nome-abrev-ini fi-nome-abrev-fin ~
bt-dig-cli bt-ex-cli fi-no-ab-reppri-ini fi-no-ab-reppri-fin bt-dig-rep ~
bt-ex-rep rs-opc-artigo bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-entrega-fin fi-nr-pedcli-ini ~
fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-matriz-ini fi-matriz-fin fi-nome-abrev-ini ~
fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin rs-opc-artigo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

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

DEFINE BUTTON bt-dig-cli 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Cliente".

DEFINE BUTTON bt-dig-item 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Itens".

DEFINE BUTTON bt-dig-mat 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Cliente".

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

DEFINE BUTTON bt-dig-rep 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Representante".

DEFINE BUTTON bt-ex-cli 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Cliente".

DEFINE BUTTON bt-ex-item 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Itens".

DEFINE BUTTON bt-ex-mat 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Cliente".

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

DEFINE BUTTON bt-ex-rep 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Representante".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia final." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-entrega-fin AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Data de Entrega M xima" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item inicial." NO-UNDO.

DEFINE VARIABLE fi-matriz-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente final." NO-UNDO.

DEFINE VARIABLE fi-matriz-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Matriz":R9 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante final." NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante inicial." NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente final." NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Cliente":R9 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido final." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido inicial." NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-94
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-95
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-96
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-97
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 36 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74.14 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-dt-entrega-fin AT ROW 1.75 COL 14 COLON-ALIGNED
     fi-nr-pedcli-ini AT ROW 2.75 COL 14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-nr-pedcli-fin AT ROW 2.75 COL 42 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     bt-dig-ped AT ROW 2.75 COL 60
     bt-ex-ped AT ROW 2.75 COL 65
     fi-it-codigo-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-it-codigo-fin AT ROW 3.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     bt-dig-item AT ROW 3.75 COL 60
     bt-ex-item AT ROW 3.75 COL 65
     fi-cod-refer-ini AT ROW 4.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-refer-fin AT ROW 4.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     bt-dig-ref AT ROW 4.75 COL 60
     bt-ex-ref AT ROW 4.75 COL 65
     fi-matriz-ini AT ROW 5.75 COL 14 COLON-ALIGNED HELP
          "Nome abreviado do cliente"
     fi-matriz-fin AT ROW 5.75 COL 42 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     bt-dig-mat AT ROW 5.75 COL 60
     bt-ex-mat AT ROW 5.75 COL 65
     fi-nome-abrev-ini AT ROW 6.75 COL 14 COLON-ALIGNED HELP
          "Nome abreviado do cliente"
     fi-nome-abrev-fin AT ROW 6.75 COL 42 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     bt-dig-cli AT ROW 6.75 COL 60
     bt-ex-cli AT ROW 6.75 COL 65
     fi-no-ab-reppri-ini AT ROW 7.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-no-ab-reppri-fin AT ROW 7.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     bt-dig-rep AT ROW 7.75 COL 60
     bt-ex-rep AT ROW 7.75 COL 65
     rs-opc-artigo AT ROW 9 COL 16.14 NO-LABEL
     bt-ok AT ROW 10.71 COL 3
     bt-cancelar AT ROW 10.71 COL 13.57
     bt-ajuda AT ROW 10.71 COL 65.57
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 3.57
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 9.86 BY .54 AT ROW 9.04 COL 6
     IMAGE-100 AT ROW 5.75 COL 31.57
     IMAGE-101 AT ROW 5.75 COL 40.57
     IMAGE-3 AT ROW 2.75 COL 31.57
     IMAGE-4 AT ROW 2.75 COL 40.57
     IMAGE-5 AT ROW 3.75 COL 31.57
     IMAGE-6 AT ROW 3.75 COL 40.57
     IMAGE-90 AT ROW 4.75 COL 31.57
     IMAGE-91 AT ROW 4.75 COL 40.57
     IMAGE-94 AT ROW 6.75 COL 31.57
     IMAGE-95 AT ROW 6.75 COL 40.57
     IMAGE-96 AT ROW 7.75 COL 31.57
     IMAGE-97 AT ROW 7.75 COL 40.57
     RECT-1 AT ROW 10.5 COL 2
     RECT-50 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.57 BY 19.79
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
         TITLE              = "Sele‡Æo de Itens de Pedido - ESSP0180a"
         HEIGHT             = 11.13
         WIDTH              = 75.86
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 1
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Sele‡Æo de Itens de Pedido - ESSP0180a */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Sele‡Æo de Itens de Pedido - ESSP0180a */
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


&Scoped-define SELF-NAME bt-dig-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-mat w-window
ON CHOOSE OF bt-dig-mat IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Matriz").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ped w-window
ON CHOOSE OF bt-dig-ped IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Pedido_de_Venda").

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


&Scoped-define SELF-NAME bt-dig-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-rep w-window
ON CHOOSE OF bt-dig-rep IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Representante").

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


&Scoped-define SELF-NAME bt-ex-mat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-mat w-window
ON CHOOSE OF bt-ex-mat IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Matriz").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ped w-window
ON CHOOSE OF bt-ex-ped IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita,
                            INPUT "E",
                            INPUT "Pedido_de_Venda").

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


&Scoped-define SELF-NAME bt-ex-rep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-rep w-window
ON CHOOSE OF bt-ex-rep IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Representante").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-entrega-fin.
    /*
    IF INT(SUBSTR(fi-dt-limite-fin,1,2)) <  1 OR
       INT(SUBSTR(fi-dt-limite-fin,1,2)) > 12  THEN DO:
        MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
        APPLY 'entry' TO fi-dt-limite-fin.
        RETURN NO-APPLY.
    END.
    IF INT(SUBSTR(fi-dt-limite-fin,4,4)) < 1 THEN DO:
        MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
        APPLY 'entry' TO fi-dt-limite-fin.
        RETURN NO-APPLY.
    END.
    */
    ASSIGN da-dt-entrega-fin  = INPUT FRAME {&FRAME-NAME} fi-dt-entrega-fin   
           c-nr-pedcli-ini    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini   
           c-nr-pedcli-fin    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin   
           c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
           c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
           c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini   
           c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin   
           c-matriz-ini       = INPUT FRAME {&FRAME-NAME} fi-matriz-ini
           c-matriz-fin       = INPUT FRAME {&FRAME-NAME} fi-matriz-fin
           c-nome-abrev-ini   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
           c-nome-abrev-fin   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin
           c-no-ab-reppri-ini = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
           c-no-ab-reppri-fin = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
           c-opc-artigo       = INPUT FRAME {&FRAME-NAME} rs-opc-artigo      
           l-ok = YES.  

    APPLY "close":U TO THIS-PROCEDURE.
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
ON LEFT-MOUSE-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
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
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
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


&Scoped-define SELF-NAME fi-matriz-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-matriz-fin w-window
ON LEAVE OF fi-matriz-fin IN FRAME F-Main
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente USE-INDEX nome WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.
      IF NOT AVAIL emitente THEN
         FIND emitente USE-INDEX codigo WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
          ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-matriz-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-matriz-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-fin
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-matriz-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-matriz-ini w-window
ON LEAVE OF fi-matriz-ini IN FRAME F-Main /* Matriz */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN DO.
     FIND emitente WHERE 
          emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
          USE-INDEX nome NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN
        FIND emitente WHERE 
             STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini 
             USE-INDEX codigo NO-LOCK NO-ERROR.

     IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.

     ASSIGN fi-matriz-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-matriz-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-matriz-ini IN FRAME F-Main /* Matriz */
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-nome-abrev-ini
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-fin
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini w-window
ON LEAVE OF fi-no-ab-reppri-ini IN FRAME F-Main /* Representante */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-no-ab-reppri-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-ini IN FRAME F-Main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-ini
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin w-window
ON LEAVE OF fi-nome-abrev-fin IN FRAME F-Main
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente USE-INDEX nome WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.

      IF NOT AVAIL emitente THEN
         FIND emitente WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin 
              NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
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
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente WHERE 
           emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
           NO-LOCK NO-ERROR.

      IF NOT AVAIL emitente THEN
         FIND emitente WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini 
              NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.

      ASSIGN fi-nome-abrev-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini w-window
ON LEAVE OF fi-nr-pedcli-ini IN FRAME F-Main /* Pedido */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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
fi-nome-abrev-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-nome-abrev-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-no-ab-reppri-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-no-ab-reppri-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-dt-entrega-fin fi-nr-pedcli-ini fi-nr-pedcli-fin fi-it-codigo-ini 
          fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-matriz-ini 
          fi-matriz-fin fi-nome-abrev-ini fi-nome-abrev-fin fi-no-ab-reppri-ini 
          fi-no-ab-reppri-fin rs-opc-artigo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE IMAGE-100 IMAGE-101 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-90 IMAGE-91 
         IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 RECT-50 fi-dt-entrega-fin 
         fi-nr-pedcli-ini fi-nr-pedcli-fin bt-dig-ped bt-ex-ped 
         fi-it-codigo-ini fi-it-codigo-fin bt-dig-item bt-ex-item 
         fi-cod-refer-ini fi-cod-refer-fin bt-dig-ref bt-ex-ref fi-matriz-ini 
         fi-matriz-fin bt-dig-mat bt-ex-mat fi-nome-abrev-ini fi-nome-abrev-fin 
         bt-dig-cli bt-ex-cli fi-no-ab-reppri-ini fi-no-ab-reppri-fin 
         bt-dig-rep bt-ex-rep rs-opc-artigo bt-ok bt-cancelar bt-ajuda 
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

  ASSIGN fi-dt-entrega-fin   = da-dt-entrega-fin
         fi-nr-pedcli-ini    = c-nr-pedcli-ini   
         fi-nr-pedcli-fin    = c-nr-pedcli-fin   
         fi-it-codigo-ini    = c-it-codigo-ini   
         fi-it-codigo-fin    = c-it-codigo-fin    
         fi-cod-refer-ini    = c-cod-refer-ini    
         fi-cod-refer-fin    = c-cod-refer-fin   
         fi-matriz-ini       = c-matriz-ini
         fi-matriz-fin       = c-matriz-fin
         fi-nome-abrev-ini   = c-nome-abrev-ini
         fi-nome-abrev-fin   = c-nome-abrev-fin
         fi-no-ab-reppri-ini = c-no-ab-reppri-ini
         fi-no-ab-reppri-fin = c-no-ab-reppri-fin
         rs-opc-artigo       = c-opc-artigo.
  
/*{utp/ut9000.i "XX9999" "9.99.99.999"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'ENTRY' TO fi-nr-pedcli-ini IN FRAME {&FRAME-NAME}.

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

