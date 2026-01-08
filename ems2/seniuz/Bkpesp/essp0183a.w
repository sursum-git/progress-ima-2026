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
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel       AS CHAR.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ped-item-ext.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ped-item-ext.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE INPUT-OUTPUT PARAMETER c-nome-abrev-fin    LIKE ped-venda.nome-abrev.
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri.
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-ini  AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-fin  AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER c-cod-depos         AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-lote-todos        AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-pp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-pd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-sc           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-ca           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER c-opc-artigo        AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-bloqueio          AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-sit-todas         AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-abe           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-atp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-att           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-pen           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-sus           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-can           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER i-credito           AS INT.
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
&Scoped-Define ENABLED-OBJECTS fi-dt-limite-ini fi-dt-limite-fin ~
fi-nr-pedcli-ini fi-nr-pedcli-fin bt-dig-ped bt-ex-ped fi-it-codigo-ini ~
fi-it-codigo-fin bt-dig-item bt-ex-item fi-cod-refer-ini fi-cod-refer-fin ~
bt-dig-ref bt-ex-ref fi-nome-abrev-ini fi-nome-abrev-fin bt-dig-cli ~
bt-ex-cli fi-no-ab-reppri-ini fi-no-ab-reppri-fin bt-dig-rep bt-ex-rep ~
fi-cod-obsoleto-ini fi-cod-obsoleto-fin bt-dig-cob bt-ex-cob ~
fi-corte-comerc-ini fi-corte-comerc-fin bt-dig-ccom bt-ex-ccom fi-cod-depos ~
tg-lote-todos rs-opc-artigo rs-bloqueio tg-sit-todas rs-credito tg-sit-atp ~
tg-sit-sus tg-sit-abe tg-sit-can tg-sit-att tg-sit-pen bt-ok bt-cancelar ~
bt-ajuda fi-cod-estabel IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-77 IMAGE-78 ~
IMAGE-83 IMAGE-87 IMAGE-88 IMAGE-90 IMAGE-91 IMAGE-94 IMAGE-95 IMAGE-96 ~
IMAGE-97 IMAGE-98 RECT-1 RECT-50 RECT-51 RECT-52 
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estabel fi-dt-limite-ini ~
fi-dt-limite-fin fi-nr-pedcli-ini fi-nr-pedcli-fin fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-nome-abrev-ini ~
fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin ~
fi-cod-obsoleto-ini fi-cod-obsoleto-fin fi-corte-comerc-ini ~
fi-corte-comerc-fin fi-cod-depos tg-lote-todos tg-lote-pp tg-lote-pd ~
tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo rs-bloqueio ~
tg-sit-todas rs-credito tg-sit-atp tg-sit-sus tg-sit-abe tg-sit-can ~
tg-sit-att tg-sit-pen fi-cod-estabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
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

DEFINE VARIABLE fi-cod-depos AS CHARACTER FORMAT "x(3)" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do dep¢sito." NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto final." NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia final." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia inicial." NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-fin AS CHARACTER FORMAT "!" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-limite-fin AS CHARACTER FORMAT "99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Data limite inicial (MM/AAAA)." NO-UNDO.

DEFINE VARIABLE fi-dt-limite-ini AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Data limite inicial (MM/AAAA)." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item inicial." NO-UNDO.

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

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
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

DEFINE IMAGE IMAGE-98
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-bloqueio AS CHARACTER INITIAL "T" 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Bloqueado", "B",
"Sem Bloqueio", "S",
"Todos", "T"
     SIZE 45 BY .79 NO-UNDO.

DEFINE VARIABLE rs-credito AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pedidos Solicitados", 1,
"Pedidos a Solicitar", 2,
"Ambos", 3
     SIZE 16 BY 2.75 NO-UNDO.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 45 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74.14 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 18.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY 3.

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
     SIZE 11.57 BY .88 TOOLTIP "Todos os Lotes (PP, PD, RP, RD, SC,CA, ETC...)" NO-UNDO.

DEFINE VARIABLE tg-sit-abe AS LOGICAL INITIAL no 
     LABEL "Aberto" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-atp AS LOGICAL INITIAL no 
     LABEL "Atendido Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-att AS LOGICAL INITIAL no 
     LABEL "Atendido Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-can AS LOGICAL INITIAL no 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-pen AS LOGICAL INITIAL no 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.72 BY .83 NO-UNDO.

DEFINE VARIABLE tg-sit-sus AS LOGICAL INITIAL no 
     LABEL "Suspenso" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-todas AS LOGICAL INITIAL no 
     LABEL "TODAS SITUA€åES" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nome-estabel AT ROW 1.75 COL 18.72 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-dt-limite-ini AT ROW 2.75 COL 14 COLON-ALIGNED
     fi-dt-limite-fin AT ROW 2.75 COL 42 COLON-ALIGNED NO-LABEL
     fi-nr-pedcli-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-nr-pedcli-fin AT ROW 3.75 COL 42 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     bt-dig-ped AT ROW 3.75 COL 60
     bt-ex-ped AT ROW 3.75 COL 65
     fi-it-codigo-ini AT ROW 4.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-it-codigo-fin AT ROW 4.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     bt-dig-item AT ROW 4.75 COL 60
     bt-ex-item AT ROW 4.75 COL 65
     fi-cod-refer-ini AT ROW 5.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-refer-fin AT ROW 5.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     bt-dig-ref AT ROW 5.75 COL 60
     bt-ex-ref AT ROW 5.75 COL 65
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
     fi-cod-obsoleto-ini AT ROW 8.75 COL 14 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 8.75 COL 42 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     bt-dig-cob AT ROW 8.75 COL 60
     bt-ex-cob AT ROW 8.75 COL 65
     fi-corte-comerc-ini AT ROW 9.75 COL 14 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 9.75 COL 42 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     bt-dig-ccom AT ROW 9.75 COL 60
     bt-ex-ccom AT ROW 9.75 COL 65
     fi-cod-depos AT ROW 10.75 COL 14 COLON-ALIGNED HELP
          "C¢digo do Dep¢sito"
     tg-lote-todos AT ROW 11.88 COL 16
     tg-lote-pp AT ROW 11.88 COL 27.86
     tg-lote-pd AT ROW 11.88 COL 33.86
     tg-lote-rp AT ROW 11.88 COL 39.86
     tg-lote-rd AT ROW 11.88 COL 45.86
     tg-lote-sc AT ROW 11.88 COL 51.86
     tg-lote-ca AT ROW 11.88 COL 57.86
     rs-opc-artigo AT ROW 12.88 COL 16.14 NO-LABEL
     rs-bloqueio AT ROW 13.96 COL 16.14 NO-LABEL
     tg-sit-todas AT ROW 16 COL 5.14
     rs-credito AT ROW 16 COL 56 NO-LABEL
     tg-sit-atp AT ROW 16.83 COL 21.57
     tg-sit-sus AT ROW 16.83 COL 42.29
     tg-sit-abe AT ROW 16.92 COL 5.14
     tg-sit-can AT ROW 17.83 COL 5.14
     tg-sit-att AT ROW 17.83 COL 21.57
     tg-sit-pen AT ROW 17.83 COL 42.29
     bt-ok AT ROW 19.58 COL 2.86
     bt-cancelar AT ROW 19.58 COL 13.86
     bt-ajuda AT ROW 19.58 COL 61.29
     fi-cod-estabel AT ROW 1.75 COL 14 COLON-ALIGNED WIDGET-ID 6
     "Situa‡Æo dos Pedidos" VIEW-AS TEXT
          SIZE 49 BY .75 AT ROW 14.96 COL 4
          BGCOLOR 9 FGCOLOR 15 FONT 6
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 3.57
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 9.86 BY .54 AT ROW 12.92 COL 5.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.57 BY 19.83
         FONT 1
         DEFAULT-BUTTON bt-ok.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Pedido a Vista:" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 14 COL 5.86
     "Situa‡Æo da Solicita‡Æo" VIEW-AS TEXT
          SIZE 20 BY .75 AT ROW 14.96 COL 54
          BGCOLOR 9 FGCOLOR 15 FONT 6
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 12 COL 12.14
     IMAGE-3 AT ROW 3.75 COL 31.57
     IMAGE-4 AT ROW 3.75 COL 40.57
     IMAGE-5 AT ROW 4.75 COL 31.57
     IMAGE-6 AT ROW 4.75 COL 40.57
     IMAGE-77 AT ROW 8.75 COL 31.72
     IMAGE-78 AT ROW 9.75 COL 31.57
     IMAGE-83 AT ROW 8.75 COL 40.57
     IMAGE-87 AT ROW 9.75 COL 40.57
     IMAGE-88 AT ROW 2.75 COL 31.57
     IMAGE-90 AT ROW 5.75 COL 31.57
     IMAGE-91 AT ROW 5.75 COL 40.57
     IMAGE-94 AT ROW 6.75 COL 31.57
     IMAGE-95 AT ROW 6.75 COL 40.57
     IMAGE-96 AT ROW 7.75 COL 31.57
     IMAGE-97 AT ROW 7.75 COL 40.57
     IMAGE-98 AT ROW 2.75 COL 40.57
     RECT-1 AT ROW 19.38 COL 1.86
     RECT-50 AT ROW 1.29 COL 2
     RECT-51 AT ROW 15.83 COL 54
     RECT-52 AT ROW 15.83 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.57 BY 19.83
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
         TITLE              = "Sele‡Æo de Itens de Pedido - ESSP0183a"
         HEIGHT             = 19.92
         WIDTH              = 75.86
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME F-Main
   5                                                                    */
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
ON END-ERROR OF w-window /* Sele‡Æo de Itens de Pedido - ESSP0183a */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Sele‡Æo de Itens de Pedido - ESSP0183a */
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
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini.

    IF INT(SUBSTR(fi-dt-limite-ini,1,2)) <  1 OR
       INT(SUBSTR(fi-dt-limite-ini,1,2)) > 12  THEN DO:
        MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
        APPLY 'entry' TO fi-dt-limite-ini.
        RETURN NO-APPLY.
    END.
    IF INT(SUBSTR(fi-dt-limite-ini,4,4)) <  1 THEN DO:
        MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
        APPLY 'entry' TO fi-dt-limite-ini.
        RETURN NO-APPLY.
    END.

    ASSIGN c-cod-estabel      = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
           c-dt-limite-ini    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini
           c-dt-limite-fin    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-fin
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
           c-corte-comerc-ini = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
           c-corte-comerc-fin = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
           c-cod-depos        = INPUT FRAME {&FRAME-NAME} fi-cod-depos
           l-lote-todos       = INPUT FRAME {&FRAME-NAME} tg-lote-todos
           l-lote-pp          = INPUT FRAME {&FRAME-NAME} tg-lote-pp
           l-lote-pd          = INPUT FRAME {&FRAME-NAME} tg-lote-pd
           l-lote-rp          = INPUT FRAME {&FRAME-NAME} tg-lote-rp
           l-lote-rd          = INPUT FRAME {&FRAME-NAME} tg-lote-rd
           l-lote-sc          = INPUT FRAME {&FRAME-NAME} tg-lote-sc
           l-lote-ca          = INPUT FRAME {&FRAME-NAME} tg-lote-ca
           c-opc-artigo       = INPUT FRAME {&FRAME-NAME} rs-opc-artigo
           c-bloqueio         = INPUT FRAME {&FRAME-NAME} rs-bloqueio
           l-sit-todas        = INPUT FRAME {&FRAME-NAME} tg-sit-todas       
           l-sit-abe          = INPUT FRAME {&FRAME-NAME} tg-sit-abe         
           l-sit-atp          = INPUT FRAME {&FRAME-NAME} tg-sit-atp         
           l-sit-att          = INPUT FRAME {&FRAME-NAME} tg-sit-att         
           l-sit-sus          = INPUT FRAME {&FRAME-NAME} tg-sit-sus         
           l-sit-can          = INPUT FRAME {&FRAME-NAME} tg-sit-can
           l-sit-pen          = INPUT FRAME {&FRAME-NAME} tg-sit-pen
           i-credito          = INPUT FRAME {&FRAME-NAME} rs-credito
           l-ok = YES.  

    APPLY "CLOSE":U TO THIS-PROCEDURE.
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
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME F-Main /* Cod.Obsoleto */
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
ON LEFT-MOUSE-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
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
  IF SELF:SCREEN-VALUE = '' THEN DO:
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ELSE
     ASSIGN fi-corte-comerc-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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
         FIND emitente USE-INDEX codigo WHERE 
              STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin NO-LOCK NO-ERROR.

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
  IF SELF:SCREEN-VALUE <> '' THEN
     FIND emitente WHERE 
          emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
          USE-INDEX nome NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN
        FIND emitente WHERE 
             STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini 
             USE-INDEX codigo NO-LOCK NO-ERROR.

     IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.

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


&Scoped-define SELF-NAME fi-nr-pedcli-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli-ini w-window
ON LEAVE OF fi-nr-pedcli-ini IN FRAME F-Main /* Pedido */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nr-pedcli-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-credito w-window
ON VALUE-CHANGED OF rs-credito IN FRAME F-Main
DO:
   IF SELF:INPUT-VALUE = 2  THEN DO.
      ASSIGN tg-sit-todas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'.
      APPLY 'value-changed' TO tg-sit-todas.
      ASSIGN tg-sit-sus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
   END.
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
            tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
  ELSE
      ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
             tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sit-att
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-att w-window
ON VALUE-CHANGED OF tg-sit-att IN FRAME F-Main /* Atendido Total */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-sit-att = YES THEN DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-can = NO THEN DO.
        ASSIGN fi-dt-limite-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
        APPLY 'entry' TO fi-dt-limite-ini.
     END.
  END.
  ELSE DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-can = NO THEN
        ASSIGN fi-dt-limite-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '010001'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sit-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-can w-window
ON VALUE-CHANGED OF tg-sit-can IN FRAME F-Main /* Cancelado */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-sit-can = YES THEN DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-att = NO THEN DO.
        ASSIGN fi-dt-limite-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
        APPLY 'entry' TO fi-dt-limite-ini.
     END.
  END.
  ELSE DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-att = NO THEN
        ASSIGN fi-dt-limite-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '010001'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sit-sus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-sus w-window
ON VALUE-CHANGED OF tg-sit-sus IN FRAME F-Main /* Suspenso */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = YES THEN DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-can = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-att = NO THEN DO.
        ASSIGN fi-dt-limite-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
        APPLY 'entry' TO fi-dt-limite-ini.
     END.
  END.
  ELSE DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-att = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-can = NO THEN
        ASSIGN fi-dt-limite-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '010001'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sit-todas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-todas w-window
ON VALUE-CHANGED OF tg-sit-todas IN FRAME F-Main /* TODAS SITUA€åES */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-sit-todas = NO THEN DO.
     ASSIGN tg-sit-abe:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-sit-atp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-sit-att:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-sit-sus:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-sit-pen:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-sit-can:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-sit-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
            tg-sit-atp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
  END.
  ELSE DO.
      ASSIGN tg-sit-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-atp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-att:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-sus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-pen:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-can:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'.

     ASSIGN tg-sit-abe:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-sit-atp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-sit-att:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-sit-sus:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-sit-pen:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-sit-can:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

     APPLY 'entry' TO fi-dt-limite-ini.
  END.
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
fi-nome-abrev-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-nome-abrev-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-no-ab-reppri-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-no-ab-reppri-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-corte-comerc-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-corte-comerc-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.


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
  DISPLAY fi-nome-estabel fi-dt-limite-ini fi-dt-limite-fin fi-nr-pedcli-ini 
          fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
          fi-cod-refer-fin fi-nome-abrev-ini fi-nome-abrev-fin 
          fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-cod-obsoleto-ini 
          fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin 
          fi-cod-depos tg-lote-todos tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd 
          tg-lote-sc tg-lote-ca rs-opc-artigo rs-bloqueio tg-sit-todas 
          rs-credito tg-sit-atp tg-sit-sus tg-sit-abe tg-sit-can tg-sit-att 
          tg-sit-pen fi-cod-estabel 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE fi-dt-limite-ini fi-dt-limite-fin fi-nr-pedcli-ini fi-nr-pedcli-fin 
         bt-dig-ped bt-ex-ped fi-it-codigo-ini fi-it-codigo-fin bt-dig-item 
         bt-ex-item fi-cod-refer-ini fi-cod-refer-fin bt-dig-ref bt-ex-ref 
         fi-nome-abrev-ini fi-nome-abrev-fin bt-dig-cli bt-ex-cli 
         fi-no-ab-reppri-ini fi-no-ab-reppri-fin bt-dig-rep bt-ex-rep 
         fi-cod-obsoleto-ini fi-cod-obsoleto-fin bt-dig-cob bt-ex-cob 
         fi-corte-comerc-ini fi-corte-comerc-fin bt-dig-ccom bt-ex-ccom 
         fi-cod-depos tg-lote-todos rs-opc-artigo rs-bloqueio tg-sit-todas 
         rs-credito tg-sit-atp tg-sit-sus tg-sit-abe tg-sit-can tg-sit-att 
         tg-sit-pen bt-ok bt-cancelar bt-ajuda fi-cod-estabel IMAGE-3 IMAGE-4 
         IMAGE-5 IMAGE-6 IMAGE-77 IMAGE-78 IMAGE-83 IMAGE-87 IMAGE-88 IMAGE-90 
         IMAGE-91 IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 IMAGE-98 RECT-1 RECT-50 
         RECT-51 RECT-52 
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

  ASSIGN fi-cod-estabel      = c-cod-estabel
         fi-dt-limite-ini    = c-dt-limite-ini
         fi-dt-limite-fin    = c-dt-limite-fin
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
         fi-corte-comerc-ini = c-corte-comerc-ini
         fi-corte-comerc-fin = c-corte-comerc-fin
         fi-cod-depos        = c-cod-depos       
         rs-opc-artigo       = c-opc-artigo
         rs-bloqueio         = c-bloqueio
         tg-lote-todos       = l-lote-todos
         tg-lote-pp          = l-lote-pp
         tg-lote-pd          = l-lote-pd
         tg-lote-rp          = l-lote-rp
         tg-lote-rd          = l-lote-rd
         tg-lote-sc          = l-lote-sc
         tg-lote-ca          = l-lote-ca
         tg-sit-todas        = l-sit-todas
         tg-sit-abe          = l-sit-abe
         tg-sit-atp          = l-sit-atp
         tg-sit-att          = l-sit-att
         tg-sit-pen          = l-sit-pen
         tg-sit-sus          = l-sit-sus
         tg-sit-can          = l-sit-can
         rs-credito          = i-credito.
  
/*{utp/ut9000.i "XX9999" "9.99.99.999"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'leave' TO fi-cod-estabel.

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

