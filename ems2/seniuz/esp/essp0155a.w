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
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-ini   AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-fin   AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER da-dt-implant-ini   AS DATE.
DEFINE INPUT-OUTPUT PARAMETER da-dt-implant-fin   AS DATE.
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
DEFINE INPUT-OUTPUT PARAMETER c-uf-ini            LIKE ped-venda.uf. 
DEFINE INPUT-OUTPUT PARAMETER c-uf-fin            LIKE ped-venda.uf.
DEFINE INPUT-OUTPUT PARAMETER c-nome-tra-ini      LIKE transporte.nome-abrev. 
DEFINE INPUT-OUTPUT PARAMETER c-nome-tra-fin      LIKE transporte.nome-abrev.
DEFINE INPUT-OUTPUT PARAMETER c-cidade-ini        LIKE ped-venda.cidade. 
DEFINE INPUT-OUTPUT PARAMETER c-cidade-fin        LIKE ped-venda.cidade.
DEFINE INPUT-OUTPUT PARAMETER i-nr-container-ini  LIKE pp-ped-venda.nr-container. 
DEFINE INPUT-OUTPUT PARAMETER i-nr-container-fin  LIKE pp-ped-venda.nr-container.
DEFINE INPUT-OUTPUT PARAMETER de-perc-comis-ini   LIKE ped-repre.perc-comis. 
DEFINE INPUT-OUTPUT PARAMETER de-perc-comis-fin   LIKE ped-repre.perc-comis.
DEFINE INPUT-OUTPUT PARAMETER i-nr-tabpre         AS INT.
DEFINE INPUT-OUTPUT PARAMETER i-situacao          AS INT.
DEFINE INPUT-OUTPUT PARAMETER i-preco             AS INT.
DEFINE INPUT-OUTPUT PARAMETER i-aprovar           AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-rubi-x            AS LOG.
DEFINE INPUT-OUTPUT PARAMETER c-tp-pedido         AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-corte             AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-todas         AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-abe           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-atp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-att           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-pen           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-sus           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-can           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER i-credito           AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-digita.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 ~
IMAGE-89 IMAGE-90 IMAGE-91 IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 ~
RECT-50 RECT-51 RECT-52 IMAGE-98 IMAGE-99 IMAGE-100 IMAGE-101 RECT-53 ~
IMAGE-102 IMAGE-103 IMAGE-104 IMAGE-105 RECT-54 RECT-55 IMAGE-106 IMAGE-107 ~
IMAGE-110 IMAGE-111 fi-cod-estabel-ini fi-cod-estabel-fin fi-dt-implant-ini ~
fi-dt-implant-fin fi-nr-pedcli-ini fi-nr-pedcli-fin bt-dig-ped bt-ex-ped ~
fi-it-codigo-ini fi-it-codigo-fin bt-dig-item bt-ex-item fi-cod-refer-ini ~
fi-cod-refer-fin bt-dig-ref bt-ex-ref fi-nome-abrev-ini fi-nome-abrev-fin ~
bt-dig-cli bt-ex-cli fi-uf-ini fi-uf-fin bt-dig-cond bt-ex-cond ~
fi-nome-tra-ini fi-nome-tra-fin bt-dig-tra bt-ex-tra fi-cidade-ini ~
fi-cidade-fin bt-dig-red bt-ex-red fi-nr-container-ini fi-nr-container-fin ~
bt-dig-container bt-ex-container fi-perc-comis-ini fi-perc-comis-fin ~
cb-tp-pedido cb-ext-tp-pedido cb-nr-tabpre tg-corte rs-situacao rs-preco ~
rs-aprovar rs-em-espera tg-sit-todas rs-credito tg-sit-atp tg-sit-sus ~
tg-sit-abe tg-sit-can tg-sit-att tg-sit-pen bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel-ini fi-cod-estabel-fin ~
fi-dt-implant-ini fi-dt-implant-fin fi-nr-pedcli-ini fi-nr-pedcli-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
fi-nome-abrev-ini fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin ~
fi-uf-ini fi-uf-fin fi-nome-tra-ini fi-nome-tra-fin fi-cidade-ini ~
fi-cidade-fin fi-nr-container-ini fi-nr-container-fin fi-perc-comis-ini ~
fi-perc-comis-fin cb-tp-pedido cb-ext-tp-pedido cb-nr-tabpre tg-tab-x ~
tg-corte rs-situacao rs-preco rs-aprovar rs-em-espera tg-sit-todas ~
rs-credito tg-sit-atp tg-sit-sus tg-sit-abe tg-sit-can tg-sit-att ~
tg-sit-pen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-5 fi-cod-estabel-ini fi-cod-estabel-fin cb-tp-pedido 

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

DEFINE BUTTON bt-dig-cond 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Cond. Pagto".

DEFINE BUTTON bt-dig-container 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Container".

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

DEFINE BUTTON bt-dig-red 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Redespacho".

DEFINE BUTTON bt-dig-ref 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Referàncias".

DEFINE BUTTON bt-dig-rep 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Representante".

DEFINE BUTTON bt-dig-tra 
     IMAGE-UP FILE "image/im-ran.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-ran.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Digita Transportadora".

DEFINE BUTTON bt-ex-cli 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Cliente".

DEFINE BUTTON bt-ex-cond 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Cond. Pagto".

DEFINE BUTTON bt-ex-container 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Container".

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

DEFINE BUTTON bt-ex-red 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Redespacho".

DEFINE BUTTON bt-ex-ref 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Referàncias".

DEFINE BUTTON bt-ex-rep 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Representante".

DEFINE BUTTON bt-ex-tra 
     IMAGE-UP FILE "image/imt-retira.bmp":U
     IMAGE-INSENSITIVE FILE "image/iit-retira.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Exceto Transportadora".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE cb-ext-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PE","PI","Todos" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE cb-nr-tabpre AS INTEGER FORMAT "9":U INITIAL 3 
     LABEL "Tabela" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Padrao",1,
                     "Rubi",2,
                     "Todas",3
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Tipo de Pedido" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Todos","Normal","Reserva","Amostra","∑ Vista","Operaá∆o Triangular","Produá∆o","Exportaá∆o","Bonificaá∆o","Doaá∆o","Bancado","Refaturamento","Amostra Exportaá∆o","Rem.Industrializaá∆o","Venda Confec." 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cidade-fin AS CHARACTER FORMAT "x(25)" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 TOOLTIP "Cidade final" NO-UNDO.

DEFINE VARIABLE fi-cidade-ini AS CHARACTER FORMAT "x(25)" 
     LABEL "Cidade" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 TOOLTIP "Cidade inicial" NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fin AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referància final." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referància inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-implant-fin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data limite final" NO-UNDO.

DEFINE VARIABLE fi-dt-implant-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Per°odo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data limite inicial" NO-UNDO.

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

DEFINE VARIABLE fi-nome-tra-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Transportadora Final" NO-UNDO.

DEFINE VARIABLE fi-nome-tra-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Transportadora" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Transportadora Inicial" NO-UNDO.

DEFINE VARIABLE fi-nr-container-fin AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Condiá∆o de Pagamento Final" NO-UNDO.

DEFINE VARIABLE fi-nr-container-ini AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Condiá∆o de Pagamento Inicial" NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido final." NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Pedido inicial." NO-UNDO.

DEFINE VARIABLE fi-perc-comis-fin AS DECIMAL FORMAT ">>9.99" INITIAL 999.99 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-comis-ini AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "% Comiss∆o" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-uf-fin AS CHARACTER FORMAT "X(256)" INITIAL "ZZZ" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Estado Final" NO-UNDO.

DEFINE VARIABLE fi-uf-ini AS CHARACTER FORMAT "X(256)" 
     LABEL "Estado (UF)" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "Estado Inicial" NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-102
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-103
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-104
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-105
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-106
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-107
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-110
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-111
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

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
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

DEFINE IMAGE IMAGE-98
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-99
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-aprovar AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Para Aprovar", 1,
"N∆o Aprovar", 2,
"Todos", 3
     SIZE 32 BY .71 NO-UNDO.

DEFINE VARIABLE rs-credito AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Aprovado", 1,
"N∆o Aprovado", 2,
"Ambos", 3
     SIZE 16 BY 2.75 NO-UNDO.

DEFINE VARIABLE rs-em-espera AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Em Espera", 1,
"Fechados", 2,
"Todos", 3
     SIZE 32 BY .96 NO-UNDO.

DEFINE VARIABLE rs-preco AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "N∆o Avaliados          .", 1,
"Aprovados                 .", 2,
"Reprovados          .", 3,
"Todos", 4
     SIZE 13 BY 3 NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Completo", 1,
"Incompleto", 2,
"Ambos", 3
     SIZE 11 BY 2.83 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 75 BY 21.5.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 22 BY 3.29.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 3.29.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 3.5.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.43 BY 3.5.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 2.25.

DEFINE VARIABLE tg-corte AS LOGICAL INITIAL no 
     LABEL "Somente Pedidos de Pilotagem" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.72 BY .83 NO-UNDO.

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
     LABEL "TODAS SITUAÄÂES (Exceto Cancelados)" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.86 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.

DEFINE VARIABLE tg-tab-x AS LOGICAL INITIAL no 
     LABEL "Somente Rubi X" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-estabel-ini AT ROW 1.75 COL 14 COLON-ALIGNED WIDGET-ID 6
     fi-cod-estabel-fin AT ROW 1.75 COL 43.29 COLON-ALIGNED NO-LABEL WIDGET-ID 130
     fi-dt-implant-ini AT ROW 2.75 COL 14 COLON-ALIGNED
     fi-dt-implant-fin AT ROW 2.75 COL 43.29 COLON-ALIGNED NO-LABEL
     fi-nr-pedcli-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-nr-pedcli-fin AT ROW 3.75 COL 43.29 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     bt-dig-ped AT ROW 3.75 COL 67
     bt-ex-ped AT ROW 3.75 COL 72
     fi-it-codigo-ini AT ROW 4.75 COL 14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-it-codigo-fin AT ROW 4.75 COL 43.29 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL
     bt-dig-item AT ROW 4.75 COL 67
     bt-ex-item AT ROW 4.75 COL 72
     fi-cod-refer-ini AT ROW 5.75 COL 14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-cod-refer-fin AT ROW 5.75 COL 43.29 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL
     bt-dig-ref AT ROW 5.75 COL 67
     bt-ex-ref AT ROW 5.75 COL 72
     fi-nome-abrev-ini AT ROW 6.75 COL 14 COLON-ALIGNED HELP
          "Nome abreviado do cliente"
     fi-nome-abrev-fin AT ROW 6.75 COL 43.29 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL
     bt-dig-cli AT ROW 6.75 COL 67
     bt-ex-cli AT ROW 6.75 COL 72
     fi-no-ab-reppri-ini AT ROW 7.75 COL 14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-no-ab-reppri-fin AT ROW 7.75 COL 43.29 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL
     bt-dig-rep AT ROW 7.75 COL 67
     bt-ex-rep AT ROW 7.75 COL 72
     fi-uf-ini AT ROW 8.75 COL 14 COLON-ALIGNED WIDGET-ID 50
     fi-uf-fin AT ROW 8.75 COL 43.29 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     bt-dig-cond AT ROW 8.75 COL 67 WIDGET-ID 44
     bt-ex-cond AT ROW 8.75 COL 72 WIDGET-ID 46
     fi-nome-tra-ini AT ROW 9.75 COL 14 COLON-ALIGNED HELP
          "Nome abreviado do cliente" WIDGET-ID 92
     fi-nome-tra-fin AT ROW 9.75 COL 43.29 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL WIDGET-ID 90
     bt-dig-tra AT ROW 9.75 COL 67 WIDGET-ID 86
     bt-ex-tra AT ROW 9.75 COL 72 WIDGET-ID 88
     fi-cidade-ini AT ROW 10.75 COL 14 COLON-ALIGNED HELP
          "Nome abreviado do cliente" WIDGET-ID 100
     fi-cidade-fin AT ROW 10.75 COL 43.29 COLON-ALIGNED HELP
          "Nome abreviado do cliente" NO-LABEL WIDGET-ID 98
     bt-dig-red AT ROW 10.75 COL 67 WIDGET-ID 94
     bt-ex-red AT ROW 10.75 COL 72 WIDGET-ID 96
     fi-nr-container-ini AT ROW 11.75 COL 14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" WIDGET-ID 62
     fi-nr-container-fin AT ROW 11.75 COL 43.29 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL WIDGET-ID 60
     bt-dig-container AT ROW 11.75 COL 67.14 WIDGET-ID 56
     bt-ex-container AT ROW 11.75 COL 72 WIDGET-ID 58
     fi-perc-comis-ini AT ROW 12.75 COL 14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" WIDGET-ID 116
     fi-perc-comis-fin AT ROW 12.75 COL 43.29 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL WIDGET-ID 114
     cb-tp-pedido AT ROW 13.75 COL 14 COLON-ALIGNED WIDGET-ID 42
     cb-ext-tp-pedido AT ROW 13.75 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.57 BY 23.63
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cb-nr-tabpre AT ROW 13.75 COL 58 COLON-ALIGNED WIDGET-ID 122
     tg-tab-x AT ROW 14.75 COL 60 WIDGET-ID 124
     tg-corte AT ROW 15.58 COL 35 WIDGET-ID 112
     rs-situacao AT ROW 15.63 COL 4.43 NO-LABEL WIDGET-ID 36
     rs-preco AT ROW 15.63 COL 19 NO-LABEL WIDGET-ID 30
     rs-aprovar AT ROW 16.75 COL 43 NO-LABEL WIDGET-ID 24
     rs-em-espera AT ROW 17.5 COL 43 NO-LABEL WIDGET-ID 70
     tg-sit-todas AT ROW 19.63 COL 5.14
     rs-credito AT ROW 19.63 COL 56 NO-LABEL
     tg-sit-atp AT ROW 20.58 COL 21.57
     tg-sit-sus AT ROW 20.58 COL 42.29
     tg-sit-abe AT ROW 20.67 COL 5.14
     tg-sit-can AT ROW 21.38 COL 5.14
     tg-sit-att AT ROW 21.38 COL 21.57
     tg-sit-pen AT ROW 21.38 COL 42.29
     bt-ok AT ROW 23.29 COL 3
     bt-cancelar AT ROW 23.29 COL 14
     bt-ajuda AT ROW 23.29 COL 66.43
     " Espera:" VIEW-AS TEXT
          SIZE 5.29 BY .54 AT ROW 17.75 COL 37 WIDGET-ID 74
     " Seleá∆o" VIEW-AS TEXT
          SIZE 8.43 BY .54 AT ROW 1 COL 3.57
          FGCOLOR 9 FONT 6
     " Aprovar:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 16.75 COL 36 WIDGET-ID 28
     " Preáo" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 15 COL 18.86 WIDGET-ID 34
     " Situaá∆o" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 15 COL 3.72 WIDGET-ID 40
     " CrÇdito" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 18.83 COL 55.43
          FGCOLOR 9 FONT 6
     " Situaá∆o dos Pedidos" VIEW-AS TEXT
          SIZE 19 BY .75 AT ROW 18.83 COL 4.86
          FGCOLOR 9 FONT 6
     IMAGE-3 AT ROW 3.75 COL 37
     IMAGE-4 AT ROW 3.75 COL 41.86
     IMAGE-5 AT ROW 4.75 COL 37
     IMAGE-6 AT ROW 4.75 COL 41.86
     IMAGE-88 AT ROW 2.75 COL 37
     IMAGE-89 AT ROW 2.75 COL 41.86
     IMAGE-90 AT ROW 5.75 COL 37
     IMAGE-91 AT ROW 5.75 COL 41.86
     IMAGE-94 AT ROW 6.75 COL 37
     IMAGE-95 AT ROW 6.75 COL 41.86
     IMAGE-96 AT ROW 7.75 COL 37
     IMAGE-97 AT ROW 7.75 COL 41.86
     RECT-1 AT ROW 23.08 COL 2
     RECT-50 AT ROW 1.25 COL 2
     RECT-51 AT ROW 19.17 COL 54
     RECT-52 AT ROW 19.17 COL 3
     IMAGE-98 AT ROW 8.75 COL 37 WIDGET-ID 52
     IMAGE-99 AT ROW 8.75 COL 41.86 WIDGET-ID 54
     IMAGE-100 AT ROW 9.75 COL 37 WIDGET-ID 64
     IMAGE-101 AT ROW 9.75 COL 41.86 WIDGET-ID 66
     RECT-53 AT ROW 15.25 COL 3 WIDGET-ID 76
     IMAGE-102 AT ROW 11.75 COL 37.14 WIDGET-ID 78
     IMAGE-103 AT ROW 11.75 COL 41.86 WIDGET-ID 80
     IMAGE-104 AT ROW 10.75 COL 37 WIDGET-ID 82
     IMAGE-105 AT ROW 10.75 COL 41.86 WIDGET-ID 84
     RECT-54 AT ROW 15.25 COL 18 WIDGET-ID 102
     RECT-55 AT ROW 16.5 COL 35 WIDGET-ID 104
     IMAGE-106 AT ROW 12.75 COL 37.14 WIDGET-ID 118
     IMAGE-107 AT ROW 12.75 COL 41.86 WIDGET-ID 120
     IMAGE-110 AT ROW 1.75 COL 37 WIDGET-ID 126
     IMAGE-111 AT ROW 1.75 COL 41.86 WIDGET-ID 128
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.57 BY 23.63
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
         TITLE              = "Seleá∆o de Itens de Pedido - ESSP0155a"
         HEIGHT             = 23.63
         WIDTH              = 76.43
         MAX-HEIGHT         = 23.63
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 23.63
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
/* SETTINGS FOR BUTTON bt-dig-rep IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ex-rep IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-tp-pedido IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-estabel-fin IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-estabel-ini IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-no-ab-reppri-fin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-no-ab-reppri-ini IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-tab-x IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Seleá∆o de Itens de Pedido - ESSP0155a */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Seleá∆o de Itens de Pedido - ESSP0155a */
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


&Scoped-define SELF-NAME bt-dig-cond
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-cond w-window
ON CHOOSE OF bt-dig-cond IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Cond_Pagto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-container w-window
ON CHOOSE OF bt-dig-container IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Container").

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


&Scoped-define SELF-NAME bt-dig-red
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-red w-window
ON CHOOSE OF bt-dig-red IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Redespacho").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-dig-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-ref w-window
ON CHOOSE OF bt-dig-ref IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Referància").

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


&Scoped-define SELF-NAME bt-dig-tra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-dig-tra w-window
ON CHOOSE OF bt-dig-tra IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                            INPUT "D",
                            INPUT "Transportadora").

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


&Scoped-define SELF-NAME bt-ex-cond
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-cond w-window
ON CHOOSE OF bt-ex-cond IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Cond_Pagto").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-container w-window
ON CHOOSE OF bt-ex-container IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Container").

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


&Scoped-define SELF-NAME bt-ex-red
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-red w-window
ON CHOOSE OF bt-ex-red IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Redespacho").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ex-ref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-ref w-window
ON CHOOSE OF bt-ex-ref IN FRAME F-Main
DO:
   RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                             INPUT "E",
                             INPUT "Referància").

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


&Scoped-define SELF-NAME bt-ex-tra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ex-tra w-window
ON CHOOSE OF bt-ex-tra IN FRAME F-Main
DO:
    RUN esdlg/d01-digita.w (INPUT-OUTPUT TABLE tt-digita, 
                              INPUT "E",
                              INPUT "Transportadora").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    ASSIGN c-cod-estabel-ini  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
           c-cod-estabel-fin  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin
           da-dt-implant-ini  = INPUT FRAME {&FRAME-NAME} fi-dt-implant-ini   
           da-dt-implant-fin  = INPUT FRAME {&FRAME-NAME} fi-dt-implant-fin   
           c-nr-pedcli-ini    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini   
           c-nr-pedcli-fin    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin   
           c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
           c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
           c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini   
           c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin   
           c-tp-pedido        = INPUT FRAME {&FRAME-NAME} cb-tp-pedido + "," + 
                                INPUT FRAME {&FRAME-NAME} cb-ext-tp-pedido
           c-nome-abrev-ini   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini
           c-nome-abrev-fin   = INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin
           c-no-ab-reppri-ini = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
           c-no-ab-reppri-fin = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
           c-uf-ini           = INPUT FRAME {&FRAME-NAME} fi-uf-ini
           c-uf-fin           = INPUT FRAME {&FRAME-NAME} fi-uf-fin
           c-nome-tra-ini     = INPUT FRAME {&FRAME-NAME} fi-nome-tra-ini
           c-nome-tra-fin     = INPUT FRAME {&FRAME-NAME} fi-nome-tra-fin
           c-cidade-ini       = INPUT FRAME {&FRAME-NAME} fi-cidade-ini
           c-cidade-fin       = INPUT FRAME {&FRAME-NAME} fi-cidade-fin
           i-nr-container-ini = INPUT FRAME {&FRAME-NAME} fi-nr-container-ini
           i-nr-container-fin = INPUT FRAME {&FRAME-NAME} fi-nr-container-fin
           de-perc-comis-ini  = INPUT FRAME {&FRAME-NAME} fi-perc-comis-ini
           de-perc-comis-fin  = INPUT FRAME {&FRAME-NAME} fi-perc-comis-fin
           i-nr-tabpre        = INPUT FRAME {&FRAME-NAME} cb-nr-tabpre
           l-corte            = INPUT FRAME {&FRAME-NAME} tg-corte
           i-situacao         = INPUT FRAME {&FRAME-NAME} rs-situacao      
           i-preco            = INPUT FRAME {&FRAME-NAME} rs-preco
           i-aprovar          = INPUT FRAME {&FRAME-NAME} rs-aprovar
           l-rubi-x           = INPUT FRAME {&FRAME-NAME} tg-tab-x
           l-sit-todas        = INPUT FRAME {&FRAME-NAME} tg-sit-todas       
           l-sit-abe          = INPUT FRAME {&FRAME-NAME} tg-sit-abe         
           l-sit-atp          = INPUT FRAME {&FRAME-NAME} tg-sit-atp         
           l-sit-att          = INPUT FRAME {&FRAME-NAME} tg-sit-att         
           l-sit-sus          = INPUT FRAME {&FRAME-NAME} tg-sit-sus         
           l-sit-can          = INPUT FRAME {&FRAME-NAME} tg-sit-can
           l-sit-pen          = INPUT FRAME {&FRAME-NAME} tg-sit-pen
           i-credito          = INPUT FRAME {&FRAME-NAME} rs-credito
           l-ok = YES.  

    APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-nr-tabpre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-nr-tabpre w-window
ON VALUE-CHANGED OF cb-nr-tabpre IN FRAME F-Main /* Tabela */
DO:
   FIND tbs_preco WHERE
        tbs_preco.tb_preco_id = INTEGER(INPUT FRAME {&FRAME-NAME} cb-nr-tabpre) 
        NO-LOCK NO-ERROR.

   ASSIGN tg-tab-x:SENSITIVE = NO
          tg-tab-x:SCREEN-VALUE = 'NO'.
   IF AVAIL tbs_preco AND
      tbs_preco.num_tipo = 2 THEN 
      ASSIGN tg-tab-x:SENSITIVE = YES.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cidade-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cidade-ini w-window
ON LEAVE OF fi-cidade-ini IN FRAME F-Main /* Cidade */
DO:
  ASSIGN SELF:SCREEN-VALUE = UPPER(SELF:SCREEN-VALUE).
  IF SELF:SCREEN-VALUE <> '' THEN 
     ASSIGN fi-cidade-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ini w-window
ON LEAVE OF fi-cod-estabel-ini IN FRAME F-Main /* Estabelecimento */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-estabel-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.

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
ON LEAVE OF fi-cod-refer-ini IN FRAME F-Main /* Referància */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-window
ON LEFT-MOUSE-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referància */
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


&Scoped-define SELF-NAME fi-no-ab-reppri-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-fin w-window
ON LEAVE OF fi-no-ab-reppri-fin IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE <> '' THEN DO.
       FIND repres WHERE 
            repres.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin NO-LOCK NO-ERROR.
       IF NOT AVAIL repres THEN
          FIND repres USE-INDEX codigo WHERE 
               STRING(repres.cod-rep) = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin NO-LOCK NO-ERROR.

       IF AVAIL repres THEN
          ASSIGN fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev.
     END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  IF SELF:SCREEN-VALUE <> '' THEN DO.
     FIND repres WHERE 
          repres.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
          USE-INDEX nome NO-LOCK NO-ERROR.
     IF NOT AVAIL repres THEN
        FIND repres WHERE 
             STRING(repres.cod-rep) = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
             USE-INDEX codigo NO-LOCK NO-ERROR.

     IF AVAIL repres THEN
        ASSIGN fi-no-ab-reppri-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = UPPER(repres.nome-abrev).

     ASSIGN fi-no-ab-reppri-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
  END.

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


&Scoped-define SELF-NAME fi-nome-tra-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tra-fin w-window
ON LEAVE OF fi-nome-tra-fin IN FRAME F-Main
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND transporte USE-INDEX nome WHERE 
           transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-tra-fin NO-LOCK NO-ERROR.
      IF NOT AVAIL transporte THEN
         FIND transporte USE-INDEX codigo WHERE 
              STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-tra-fin NO-LOCK NO-ERROR.

      IF AVAIL transporte THEN
          ASSIGN fi-nome-tra-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tra-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nome-tra-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z01ad268.w
                       &campo     = fi-nome-tra-fin
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-tra-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tra-ini w-window
ON LEAVE OF fi-nome-tra-ini IN FRAME F-Main /* Transportadora */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN DO.
     FIND transporte WHERE 
          transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-tra-ini
          USE-INDEX nome NO-LOCK NO-ERROR.
     IF NOT AVAIL transporte THEN
        FIND transporte WHERE 
             STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-tra-ini 
             USE-INDEX codigo NO-LOCK NO-ERROR.

     IF AVAIL transporte THEN
        ASSIGN fi-nome-tra-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.

     ASSIGN fi-nome-tra-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tra-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nome-tra-ini IN FRAME F-Main /* Transportadora */
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z01ad268.w
                       &campo     = fi-nome-tra-ini
                       &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-container-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nr-container-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = eszoom\z01pp001.w
                       &campo     = fi-nr-container-fin
                       &campozoom = nr-container}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-container-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container-ini w-window
ON LEAVE OF fi-nr-container-ini IN FRAME F-Main /* Container */
DO:
  IF SELF:SCREEN-VALUE <> '0' THEN
     ASSIGN fi-nr-container-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-nr-container-ini IN FRAME F-Main /* Container */
DO:
    {include/zoomvar.i &prog-zoom = eszoom\z01pp001.w
                       &campo     = fi-nr-container-ini
                       &campozoom = nr-container}
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


&Scoped-define SELF-NAME fi-perc-comis-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-comis-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-perc-comis-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = eszoom\z01pp001.w
                       &campo     = fi-nr-container-fin
                       &campozoom = nr-container}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc-comis-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-comis-ini w-window
ON LEAVE OF fi-perc-comis-ini IN FRAME F-Main /* % Comiss∆o */
DO:
  IF SELF:SCREEN-VALUE <> '0' THEN
     ASSIGN fi-perc-comis-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc-comis-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-perc-comis-ini IN FRAME F-Main /* % Comiss∆o */
DO:
    {include/zoomvar.i &prog-zoom = eszoom\z01pp001.w
                       &campo     = fi-nr-container-ini
                       &campozoom = nr-container}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-uf-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-uf-ini w-window
ON LEAVE OF fi-uf-ini IN FRAME F-Main /* Estado (UF) */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-uf-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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


&Scoped-define SELF-NAME tg-sit-att
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-att w-window
ON VALUE-CHANGED OF tg-sit-att IN FRAME F-Main /* Atendido Total */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-sit-att = YES THEN DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-can = NO THEN DO.
        ASSIGN fi-dt-implant-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
        APPLY 'entry' TO fi-dt-implant-ini.
     END.
  END.
  ELSE DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-sus = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-can = NO THEN
        ASSIGN fi-dt-implant-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '010001'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sit-can
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-can w-window
ON VALUE-CHANGED OF tg-sit-can IN FRAME F-Main /* Cancelado */
DO:
   IF SELF:SCREEN-VALUE = 'YES' THEN 
      ASSIGN tg-sit-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-atp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-att:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-sus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-sit-pen:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'.
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
        ASSIGN fi-dt-implant-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
        APPLY 'entry' TO fi-dt-implant-ini.
     END.
  END.
  ELSE DO.
     IF INPUT FRAME {&FRAME-NAME} tg-sit-att = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-pen = NO AND
        INPUT FRAME {&FRAME-NAME} tg-sit-can = NO THEN
        ASSIGN fi-dt-implant-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '010001'.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sit-todas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-todas w-window
ON VALUE-CHANGED OF tg-sit-todas IN FRAME F-Main /* TODAS SITUAÄÂES (Exceto Cancelados) */
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

  END.
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
fi-uf-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-uf-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-nr-container-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-nr-container-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-cod-estabel-ini fi-cod-estabel-fin fi-dt-implant-ini 
          fi-dt-implant-fin fi-nr-pedcli-ini fi-nr-pedcli-fin fi-it-codigo-ini 
          fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-nome-abrev-ini 
          fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-uf-ini 
          fi-uf-fin fi-nome-tra-ini fi-nome-tra-fin fi-cidade-ini fi-cidade-fin 
          fi-nr-container-ini fi-nr-container-fin fi-perc-comis-ini 
          fi-perc-comis-fin cb-tp-pedido cb-ext-tp-pedido cb-nr-tabpre tg-tab-x 
          tg-corte rs-situacao rs-preco rs-aprovar rs-em-espera tg-sit-todas 
          rs-credito tg-sit-atp tg-sit-sus tg-sit-abe tg-sit-can tg-sit-att 
          tg-sit-pen 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 IMAGE-89 IMAGE-90 IMAGE-91 
         IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 RECT-50 RECT-51 RECT-52 
         IMAGE-98 IMAGE-99 IMAGE-100 IMAGE-101 RECT-53 IMAGE-102 IMAGE-103 
         IMAGE-104 IMAGE-105 RECT-54 RECT-55 IMAGE-106 IMAGE-107 IMAGE-110 
         IMAGE-111 fi-cod-estabel-ini fi-cod-estabel-fin fi-dt-implant-ini 
         fi-dt-implant-fin fi-nr-pedcli-ini fi-nr-pedcli-fin bt-dig-ped 
         bt-ex-ped fi-it-codigo-ini fi-it-codigo-fin bt-dig-item bt-ex-item 
         fi-cod-refer-ini fi-cod-refer-fin bt-dig-ref bt-ex-ref 
         fi-nome-abrev-ini fi-nome-abrev-fin bt-dig-cli bt-ex-cli fi-uf-ini 
         fi-uf-fin bt-dig-cond bt-ex-cond fi-nome-tra-ini fi-nome-tra-fin 
         bt-dig-tra bt-ex-tra fi-cidade-ini fi-cidade-fin bt-dig-red bt-ex-red 
         fi-nr-container-ini fi-nr-container-fin bt-dig-container 
         bt-ex-container fi-perc-comis-ini fi-perc-comis-fin cb-tp-pedido 
         cb-ext-tp-pedido cb-nr-tabpre tg-corte rs-situacao rs-preco rs-aprovar 
         rs-em-espera tg-sit-todas rs-credito tg-sit-atp tg-sit-sus tg-sit-abe 
         tg-sit-can tg-sit-att tg-sit-pen bt-ok bt-cancelar bt-ajuda 
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

  ASSIGN fi-cod-estabel-ini  = c-cod-estabel-ini
         fi-cod-estabel-fin  = c-cod-estabel-fin
         fi-dt-implant-ini   = da-dt-implant-ini   
         fi-dt-implant-fin   = da-dt-implant-fin
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
         fi-uf-ini           = c-uf-ini
         fi-uf-fin           = c-uf-fin
         fi-nome-tra-ini     = c-nome-tra-ini
         fi-nome-tra-fin     = c-nome-tra-fin
         fi-cidade-ini       = c-cidade-ini
         fi-cidade-fin       = c-cidade-fin
         fi-nr-container-ini = i-nr-container-ini
         fi-nr-container-fin = i-nr-container-fin
         cb-nr-tabpre        = i-nr-tabpre
         cb-tp-pedido        = ENTRY(1,c-tp-pedido)
         cb-ext-tp-pedido    = ENTRY(2,c-tp-pedido)
         rs-situacao         = i-situacao
         rs-preco            = i-preco
         rs-aprovar          = i-aprovar
         tg-tab-x            = l-rubi-x
         tg-corte            = l-corte
         tg-sit-todas        = l-sit-todas
         tg-sit-abe          = l-sit-abe
         tg-sit-atp          = l-sit-atp
         tg-sit-att          = l-sit-att
         tg-sit-pen          = l-sit-pen
         tg-sit-sus          = l-sit-sus
         tg-sit-can          = l-sit-can
         rs-credito          = i-credito.
  
/*{utp/ut9000.i "XX9999" "9.99.99.999"} */

  FIND usuar_mestre WHERE
       usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

  ASSIGN fi-no-ab-reppri-ini = ''
         fi-no-ab-reppri-fin = 'ZZZZZZZZZZZZZZ'.
  ASSIGN fi-no-ab-reppri-ini:SENSITIVE = YES
         fi-no-ab-reppri-fin:SENSITIVE = YES.

  FIND FIRST repres WHERE
             SUBSTRING(repres.char-1,500,12) = c-seg-usuario AND 
             repres.ind-sit = 1 NO-LOCK NO-ERROR.

  IF NOT AVAIL repres THEN
     FIND repres WHERE
          repres.nome = usuar_mestre.nom_usuar NO-LOCK NO-ERROR.

  IF AVAIL repres THEN  DO.
     FIND cm-ext-repres WHERE
          cm-ext-repres.cod-rep = repres.cod-rep NO-ERROR.

     IF NOT AVAIL cm-ext-repres OR
        (AVAIL cm-ext-repres AND cm-ext-repres.classe > 2) THEN DO.
        ASSIGN fi-no-ab-reppri-ini = repres.nome-abrev
               fi-no-ab-reppri-fin = repres.nome-abrev.

        ASSIGN fi-no-ab-reppri-ini:SENSITIVE = NO
               fi-no-ab-reppri-fin:SENSITIVE = NO.

     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'VALUE-CHANGED' TO cb-nr-tabpre.
  APPLY 'entry' TO fi-cod-estabel-ini.

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

