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

/* Local Variable Definitions ---                                       */
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel       AS   CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite         AS   CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-fin    LIKE ped-venda.nome-abrev.
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ped-item-ext.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ped-item-ext.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-ini  AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-fin  AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER de-perc-min         AS DEC.
DEFINE INPUT-OUTPUT PARAMETER de-perc-max         AS DEC.
DEFINE INPUT-OUTPUT PARAMETER c-tp-artigo         AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-lote-todos        AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-pp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-pd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-sc           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-ca           AS LOG.
DEFINE OUTPUT       PARAMETER l-ok                AS LOG.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-106 IMAGE-107 IMAGE-2 IMAGE-3 IMAGE-4 ~
IMAGE-51 IMAGE-52 IMAGE-73 IMAGE-77 IMAGE-83 IMAGE-94 IMAGE-95 IMAGE-96 ~
IMAGE-97 RECT-1 RECT-49 fi-dt-limite fi-nr-pedcli-ini fi-nr-pedcli-fin ~
fi-nome-abrev-ini fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
tg-lote-todos rs-opc-artigo bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-nome-estabel ~
fi-dt-limite fi-nr-pedcli-ini fi-nr-pedcli-fin fi-nome-abrev-ini ~
fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-perc-min fi-perc-max tg-lote-todos tg-lote-pp ~
tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin 
&Scoped-define List-5 fi-cod-estabel 

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

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
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

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido final." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido inicial." NO-UNDO.

DEFINE VARIABLE fi-perc-max AS DECIMAL FORMAT ">9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-min AS DECIMAL FORMAT "->9.99" INITIAL 0 
     LABEL "Tolerƒncia" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE IMAGE IMAGE-106
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-107
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
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

DEFINE IMAGE IMAGE-83
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

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
     SIZE 36.29 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 66.29 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 12.25.

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
     fi-cod-estabel AT ROW 1.75 COL 15 COLON-ALIGNED WIDGET-ID 6
     fi-nome-estabel AT ROW 1.75 COL 19.72 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-dt-limite AT ROW 2.75 COL 15 COLON-ALIGNED
     fi-nr-pedcli-ini AT ROW 3.75 COL 15 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-nr-pedcli-fin AT ROW 3.75 COL 43.86 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     fi-nome-abrev-ini AT ROW 4.75 COL 15 COLON-ALIGNED HELP
          "Nome abreviado do cliente"
     fi-nome-abrev-fin AT ROW 4.75 COL 43.86 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     fi-no-ab-reppri-ini AT ROW 5.75 COL 15 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-no-ab-reppri-fin AT ROW 5.75 COL 43.86 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-it-codigo-ini AT ROW 6.75 COL 15 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 6.75 COL 43.86 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 7.75 COL 15 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 7.75 COL 43.86 COLON-ALIGNED NO-LABEL
     fi-cod-obsoleto-ini AT ROW 8.75 COL 15 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 8.75 COL 43.86 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-perc-min AT ROW 9.75 COL 15 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-perc-max AT ROW 9.75 COL 43.86 COLON-ALIGNED NO-LABEL
     tg-lote-todos AT ROW 11 COL 17.43
     tg-lote-pp AT ROW 11 COL 29.29
     tg-lote-pd AT ROW 11 COL 35.29
     tg-lote-rp AT ROW 11 COL 41.29
     tg-lote-rd AT ROW 11 COL 47.29
     tg-lote-sc AT ROW 11 COL 53.29
     tg-lote-ca AT ROW 11 COL 59.29
     rs-opc-artigo AT ROW 12 COL 17.29 NO-LABEL
     bt-ok AT ROW 13.75 COL 2.72
     bt-cancelar AT ROW 13.75 COL 13.72
     bt-ajuda AT ROW 13.75 COL 56.43
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 11.13 COL 13.29
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 11.88 COL 7
     IMAGE-106 AT ROW 9.75 COL 34
     IMAGE-107 AT ROW 9.75 COL 42
     IMAGE-2 AT ROW 6.75 COL 42
     IMAGE-3 AT ROW 3.75 COL 34
     IMAGE-4 AT ROW 3.75 COL 42
     IMAGE-51 AT ROW 7.75 COL 34
     IMAGE-52 AT ROW 7.75 COL 42
     IMAGE-73 AT ROW 6.75 COL 34
     IMAGE-77 AT ROW 8.75 COL 34.14
     IMAGE-83 AT ROW 8.75 COL 42.14
     IMAGE-94 AT ROW 4.75 COL 34
     IMAGE-95 AT ROW 4.75 COL 42
     IMAGE-96 AT ROW 5.75 COL 34
     IMAGE-97 AT ROW 5.75 COL 42
     RECT-1 AT ROW 13.54 COL 1.72
     RECT-49 AT ROW 1.13 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.92
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
         TITLE              = "Parƒmetros Faturamento Autom tico - ESSP0154a"
         HEIGHT             = 14.04
         WIDTH              = 68.14
         MAX-HEIGHT         = 29.13
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.13
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-cod-obsoleto-fin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-obsoleto-ini IN FRAME F-Main
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN fi-perc-max IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-min IN FRAME F-Main
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
ON END-ERROR OF w-window /* Parƒmetros Faturamento Autom tico - ESSP0154a */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Parƒmetros Faturamento Autom tico - ESSP0154a */
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
  apply "close":U to this-procedure.
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
         c-nr-pedcli-ini    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini   
         c-nr-pedcli-fin    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin   
         c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
         c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin
         c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
         c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
         c-nome-abrev-ini   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
         c-nome-abrev-fin   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin
         c-no-ab-reppri-ini = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
         c-no-ab-reppri-fin = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
         c-cod-obsoleto-ini = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini
         c-cod-obsoleto-fin = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
         de-perc-min        = INPUT FRAME {&FRAME-NAME} fi-perc-min       
         de-perc-max        = INPUT FRAME {&FRAME-NAME} fi-perc-max       
         l-lote-todos       = INPUT FRAME {&FRAME-NAME} tg-lote-todos
         l-lote-pp          = INPUT FRAME {&FRAME-NAME} tg-lote-pp
         l-lote-pd          = INPUT FRAME {&FRAME-NAME} tg-lote-pd
         l-lote-rp          = INPUT FRAME {&FRAME-NAME} tg-lote-rp
         l-lote-rd          = INPUT FRAME {&FRAME-NAME} tg-lote-rd
         l-lote-sc          = INPUT FRAME {&FRAME-NAME} tg-lote-sc
         l-lote-ca          = INPUT FRAME {&FRAME-NAME} tg-lote-ca
         c-tp-artigo        = INPUT FRAME {&FRAME-NAME} rs-opc-artigo
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
  IF AVAIL estabelec THEN
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
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
ON LEAVE OF fi-it-codigo-fin IN FRAME F-Main
DO:
    ASSIGN fi-perc-min:SENSITIVE = NO
           fi-perc-max:SENSITIVE = NO.

    IF INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini AND
       INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini THEN
       ASSIGN fi-perc-min:SENSITIVE = YES
              fi-perc-max:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nome-abrev-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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


&Scoped-define SELF-NAME fi-nr-pedcli-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-fin w-window
ON LEAVE OF fi-nr-pedcli-fin IN FRAME F-Main
DO:
  ASSIGN fi-perc-min:SENSITIVE = NO
         fi-perc-max:SENSITIVE = NO.

  IF INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini AND
     INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini THEN
     ASSIGN fi-perc-min:SENSITIVE = YES
            fi-perc-max:SENSITIVE = YES.

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


&Scoped-define SELF-NAME fi-perc-min
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min w-window
ON LEAVE OF fi-perc-min IN FRAME F-Main /* Tolerƒncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-min w-window
ON VALUE-CHANGED OF fi-perc-min IN FRAME F-Main /* Tolerƒncia */
DO:
  ASSIGN SELF:FGCOLOR = 0.
  IF SELF:INPUT-VALUE < 0 THEN
     ASSIGN SELF:FGCOLOR = 12.
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
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-rp).
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
fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-cod-estabel fi-nome-estabel fi-dt-limite fi-nr-pedcli-ini 
          fi-nr-pedcli-fin fi-nome-abrev-ini fi-nome-abrev-fin 
          fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-it-codigo-ini 
          fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-cod-obsoleto-ini 
          fi-cod-obsoleto-fin fi-perc-min fi-perc-max tg-lote-todos tg-lote-pp 
          tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE IMAGE-106 IMAGE-107 IMAGE-2 IMAGE-3 IMAGE-4 IMAGE-51 IMAGE-52 IMAGE-73 
         IMAGE-77 IMAGE-83 IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 RECT-49 
         fi-dt-limite fi-nr-pedcli-ini fi-nr-pedcli-fin fi-nome-abrev-ini 
         fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin 
         fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
         tg-lote-todos rs-opc-artigo bt-ok bt-cancelar bt-ajuda 
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
  
  ASSIGN fi-cod-estabel:SENSITIVE = NO.
  IF c-cod-estabel = "0" THEN DO:
     ASSIGN fi-cod-estabel:SENSITIVE = YES.
     ASSIGN c-cod-estabel = '1'.
  END.

  ASSIGN fi-cod-estabel      = c-cod-estabel
         fi-dt-limite        = c-dt-limite
         fi-nr-pedcli-ini    = c-nr-pedcli-ini   
         fi-nr-pedcli-fin    = c-nr-pedcli-fin   
         fi-it-codigo-ini    = c-it-codigo-ini   
         fi-it-codigo-fin    = c-it-codigo-fin    
         fi-cod-refer-ini    = c-cod-refer-ini    
         fi-cod-refer-fin    = c-cod-refer-fin   
         fi-nome-abrev-ini   = c-nome-abrev-ini
         fi-nome-abrev-fin   = c-nome-abrev-fin
         fi-no-ab-reppri-ini = c-no-ab-reppri-ini
         fi-no-ab-reppri-fin = c-no-ab-reppri-fin
         fi-cod-obsoleto-ini = c-cod-obsoleto-ini
         fi-cod-obsoleto-fin = c-cod-obsoleto-fin
         fi-perc-min         = de-perc-min
         fi-perc-max         = de-perc-max
         rs-opc-artigo       = c-tp-artigo
         tg-lote-todos       = l-lote-todos
         tg-lote-pp          = l-lote-pp
         tg-lote-pd          = l-lote-pd
         tg-lote-rp          = l-lote-rp
         tg-lote-rd          = l-lote-rd
         tg-lote-sc          = l-lote-sc
         tg-lote-ca          = l-lote-ca.

 /* {utp/ut9000.i "XX9999" "9.99.99.999"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'leave' TO fi-cod-estabel IN FRAME {&FRAME-NAME}.

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

