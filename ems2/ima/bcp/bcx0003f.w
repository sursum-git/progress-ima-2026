&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2med          PROGRESS
*/
&Scoped-define WINDOW-NAME wgEmbarque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wgEmbarque 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i BCX0003F 2.00.00.000}  /*** 010000 ***/
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */


CREATE WIDGET-POOL.

{include/i_dbvers.i}

/* ***************************  Definitions  ************************** */
/*
def var c-nom-prog-upc-mg97  as char format "x(50)" no-undo.
def var c-nom-prog-appc-mg97 as char format "x(50)" no-undo.
def var c-nom-prog-dpc-mg97  as char format "x(50)" no-undo.
def var c-ctrl-tab           as char                no-undo.
def var h-ctrl-tab           as handle              no-undo.
def var wh-entry-field       as widget-handle       no-undo.
*/
{include/i-sysvar.i}
{utp/ut-glob.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{ftp/ftapi400.i}
{ftp/ftapi401.i}
{bcp/bc9102.i}

Define Temp-table tt-confere-aviso no-undo
    Field nr-embarque       As Integer   Format '>>>>>>>>>9' Label 'Embarque'
    Field nr-resumo         as integer
    Field cod-item          As Character Format 'x(16)'      Label 'Item'
    Field cod-refer         As Character Format 'x(10)'      Label 'Referencia'
    Field des-item          As Character Format 'x(30)'      Label 'Descriá∆o'
    Field qtd-faturada      As Decimal                       Label 'Qtde Faturada'
    Field qtd-embarcada     As Decimal                       Label 'Qtde Embarcada'
    Field qtd-aberto        As Decimal                       Label 'Qtde Em Aberto'
    Index Aberto qtd-aberto Descending.

Define Variable chTick          As Com-Handle               No-undo.
define var      de-total-item   as decimal decimals 4       no-undo.

DEF VAR C-ITEM-FALTA      AS CHAR FORMA  "X(16)" NO-UNDO.
DEF VAR DE-QT-LIDO        AS DEC  DECIMALS 10    NO-UNDO.
DEF VAR DE-QT-TOTAL       AS DEC  DECIMALS 10    NO-UNDO. 
                                                    
DEF VAR h_bcx1000 AS HANDLE             NO-UNDO.

def var i-time as int.
def var vUsuar as char format "x(12)".
def var vnrresumo as int.
def var c-transacao as char no-undo.
def var h-acomp as handle no-undo.

def buffer b-tt-pre-fatur for tt-pre-fatur.
DEF BUFFER B-BC-ETIQUETA FOR BC-ETIQUETA.

DEF TEMP-TABLE tt-it-pre-fat-Bo  NO-UNDO LIKE it-pre-fat
    FIELD qt-coletada AS DECIMAL DECIMALS 10
    INDEX ch-pedido IS PRIMARY cdd-embarq
                                nr-resumo 
                                nome-abrev 
                                nr-pedcli 
                                nr-sequencia 
                                it-codigo 
                                cod-refer 
                                nr-entrega.

def var l-sit-resumo-1 as log no-undo.
def var l-sit-resumo-2 as log no-undo.
def var l-sit-resumo-3 as log no-undo.
def var l-sit-resumo-4 as log no-undo.
def var i-sit-resumo   as int no-undo.
def var l-mostra-item as log no-undo.
def var c-atualizando as char no-undo.
def var c-ultima-atualizacao as char no-undo.
def var c-fechado           as char no-undo.
def var c-aberto            as char no-undo.
def var c-Fechar            as char no-undo.
def var c-Abrir             as char no-undo.
def var i-cont              as int.
DEF VAR l-erro-emb  AS LOG NO-UNDO.
DEF VAR c-mess-erro AS CHAR NO-UNDO.


DEFINE VARIABLE arq-etq-retiradas-dtl AS CHARACTER   NO-UNDO.
DEFINE VARIABLE arq-etq-retiradas-smp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE var-qt-etq-sel        AS INTEGER     NO-UNDO.
DEFINE VARIABLE var-abrir-relat-etq   AS LOGICAL     NO-UNDO.


DEF STREAM eqt-dtl.
DEF STREAM eqt-smp.

Define Variable vLogFechado         As Logical          No-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fEmbarque
&Scoped-define BROWSE-NAME bConfere

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-confere-aviso saldo-estoq tt-pre-fatur

/* Definitions for BROWSE bConfere                                      */
&Scoped-define FIELDS-IN-QUERY-bConfere tt-confere-aviso.cod-item tt-confere-aviso.cod-refer tt-confere-aviso.des-item tt-confere-aviso.qtd-faturada tt-confere-aviso.qtd-embarcada tt-confere-aviso.qtd-aberto (saldo-estoq.qtidade-atu - saldo-estoq.qt-aloc-prod - saldo-estoq.qt-aloc-ped - saldo-estoq.qt-alocada) @ saldo-estoq.qtidade-atu   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bConfere   
&Scoped-define SELF-NAME bConfere
&Scoped-define QUERY-STRING-bConfere FOR EACH tt-confere-aviso, ~
                               FIRST saldo-estoq NO-LOCK                         WHERE saldo-estoq.it-codigo = tt-confere-aviso.cod-item                           AND saldo-estoq.cod-refer = tt-confere-aviso.cod-refer                           /*AND saldo-estoq.cod-depos = "ARM" */
&Scoped-define OPEN-QUERY-bConfere OPEN QUERY bConfere FOR EACH tt-confere-aviso, ~
                               FIRST saldo-estoq NO-LOCK                         WHERE saldo-estoq.it-codigo = tt-confere-aviso.cod-item                           AND saldo-estoq.cod-refer = tt-confere-aviso.cod-refer                           /*AND saldo-estoq.cod-depos = "ARM" */ .
&Scoped-define TABLES-IN-QUERY-bConfere tt-confere-aviso saldo-estoq
&Scoped-define FIRST-TABLE-IN-QUERY-bConfere tt-confere-aviso
&Scoped-define SECOND-TABLE-IN-QUERY-bConfere saldo-estoq


/* Definitions for BROWSE bresumo                                       */
&Scoped-define FIELDS-IN-QUERY-bresumo tt-pre-fatur.cdd-embarq tt-pre-fatur.nr-resumo tt-pre-fatur.dt-embarque tt-pre-fatur.nome-abrev tt-pre-fatur.motorista tt-pre-fatur.placa tt-pre-fatur.uf-placa vUsuar   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bresumo   
&Scoped-define SELF-NAME bresumo
&Scoped-define QUERY-STRING-bresumo FOR EACH tt-pre-fatur
&Scoped-define OPEN-QUERY-bresumo OPEN QUERY bresumo FOR EACH tt-pre-fatur.
&Scoped-define TABLES-IN-QUERY-bresumo tt-pre-fatur
&Scoped-define FIRST-TABLE-IN-QUERY-bresumo tt-pre-fatur


/* Definitions for FRAME fEmbarque                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fEmbarque ~
    ~{&OPEN-QUERY-bConfere}~
    ~{&OPEN-QUERY-bresumo}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-10 RECT-11 RECT-12 RECT-13 ~
RECT-14 RECT-2 RECT-4 RECT-6 RECT-8 BT-SAIR btEtqEmb vNumAviso vNumAvisofim ~
btConfere vtresumo btLimpa vLigado c-hora bresumo btlistaitem btLegenda ~
btlimpaitem bConfere bt-relat-etiquetas 
&Scoped-Define DISPLAYED-OBJECTS vNumAviso vNumAvisofim vtresumo ~
c-sit-embarque vnrresumo-ini vnrresumo-fim vLigado vDesStatusEmbarque ~
vIntervalo c-hora de-qt-volume de-qt-peso de-qt-metro 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wgEmbarque AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Aviso_de_Embarque 
       MENU-ITEM m_Conferir     LABEL "&Conferir"     
       MENU-ITEM m_Sair         LABEL "&Sair"         .

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM m_Topicos_da_Ajuda LABEL "T¢picos da Ajuda"
       MENU-ITEM m_Sobre        LABEL "Sobre"         .

DEFINE MENU MENU-BAR-wgEmbarque MENUBAR
       SUB-MENU  m_Aviso_de_Embarque LABEL "&Embarque"     
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-relat-etiquetas 
     LABEL "&Salvar Etiq" 
     SIZE 11.72 BY 1.5.

DEFINE BUTTON BT-SAIR 
     IMAGE-UP FILE "IMAGE/im-exi.bmp":U
     LABEL "SAIR" 
     SIZE 11.72 BY 1.25.

DEFINE BUTTON btConfere 
     IMAGE-UP FILE "image/im-enter":U
     LABEL "Confere" 
     SIZE 6.43 BY 1.42.

DEFINE BUTTON btEtqEmb 
     IMAGE-UP FILE "image/im-classe.bmp":U
     LABEL "Imprime Etq Emb" 
     SIZE 6.43 BY 1.42 TOOLTIP "Imprime Etiquetas de Embalagem".

DEFINE BUTTON btFechaAviso 
     IMAGE-UP FILE "image/im-smfec":U
     IMAGE-INSENSITIVE FILE "image/ii-smfec":U
     LABEL "Fecha" 
     SIZE 7.14 BY 1.67.

DEFINE BUTTON btLegenda 
     LABEL "&Legenda" 
     SIZE 11.72 BY 1.5.

DEFINE BUTTON btLimpa 
     IMAGE-UP FILE "image/im-era":U
     IMAGE-INSENSITIVE FILE "image/ii-era":U
     LABEL "Limpa" 
     SIZE 7.14 BY 1.67 TOOLTIP "Reinicia Resumo Embarque".

DEFINE BUTTON btlimpaitem 
     LABEL "&Retirar Item" 
     SIZE 11.72 BY 1.5.

DEFINE BUTTON btlistaitem 
     LABEL "&Lista item" 
     SIZE 11.72 BY 1.5.

DEFINE VARIABLE c-hora AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE c-sit-embarque AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE de-qt-metro AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     LABEL "Total Metros" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE de-qt-peso AS DECIMAL FORMAT "->>,>>9.9999":U INITIAL 0 
     LABEL "Total Peso" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE de-qt-volume AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Volumes" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE vDesStatusEmbarque AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE vIntervalo AS INTEGER FORMAT ">>9":U INITIAL 10 
     LABEL "Tempo Atualizaá∆o (segs)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE vnrresumo-fim AS INTEGER FORMAT ">>>>>9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "N£mero do embarque" NO-UNDO.

DEFINE VARIABLE vnrresumo-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "N£mero Resumo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "N£mero do embarque" NO-UNDO.

DEFINE VARIABLE vNumAviso AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Embarque" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "N£mero do Embarque" NO-UNDO.

DEFINE VARIABLE vNumAvisofim AS INTEGER FORMAT ">>>>>>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "N£mero do Embarque" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 5.33.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 4.25.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 1.5.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 1.5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 1.5.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 1.75.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 8.58.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 5.25.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 2.17.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 12.

DEFINE VARIABLE vLigado AS LOGICAL INITIAL no 
     LABEL "At&ualizar Automaticamente" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 NO-UNDO.

DEFINE VARIABLE vtresumo AS LOGICAL INITIAL yes 
     LABEL "Todos os Resumos" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bConfere FOR 
      tt-confere-aviso, 
      saldo-estoq SCROLLING.

DEFINE QUERY bresumo FOR 
      tt-pre-fatur SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bConfere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bConfere wgEmbarque _FREEFORM
  QUERY bConfere DISPLAY
      tt-confere-aviso.cod-item
    tt-confere-aviso.cod-refer
    tt-confere-aviso.des-item
    tt-confere-aviso.qtd-faturada
    tt-confere-aviso.qtd-embarcada
    tt-confere-aviso.qtd-aberto
    (saldo-estoq.qtidade-atu - saldo-estoq.qt-aloc-prod - 
     saldo-estoq.qt-aloc-ped - saldo-estoq.qt-alocada) @ saldo-estoq.qtidade-atu FORMAT "->,>>>,>>>9.9999" COLUMN-LABEL "Qt Dispon°vel"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 79 BY 8.08
         FONT 1 ROW-HEIGHT-CHARS .79.

DEFINE BROWSE bresumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bresumo wgEmbarque _FREEFORM
  QUERY bresumo DISPLAY
      tt-pre-fatur.cdd-embarq format ">>>>>>9"
tt-pre-fatur.nr-resumo   format ">>>>>9" 
tt-pre-fatur.dt-embarque format "99/99/9999"
tt-pre-fatur.nome-abrev
tt-pre-fatur.motorista
tt-pre-fatur.placa
tt-pre-fatur.uf-placa
vUsuar
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX NO-ROW-MARKERS SEPARATORS SIZE 79 BY 4.58
         FONT 1 ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fEmbarque
     BT-SAIR AT ROW 1.58 COL 89.72
     btEtqEmb AT ROW 1.75 COL 72
     vNumAviso AT ROW 1.83 COL 23.72 COLON-ALIGNED
     vNumAvisofim AT ROW 1.83 COL 34.72 COLON-ALIGNED NO-LABEL
     btConfere AT ROW 1.83 COL 47.57
     vtresumo AT ROW 2.88 COL 25.72
     c-sit-embarque AT ROW 3.67 COL 81.43 COLON-ALIGNED NO-LABEL
     vnrresumo-ini AT ROW 3.75 COL 23.72 COLON-ALIGNED
     vnrresumo-fim AT ROW 3.75 COL 34.72 COLON-ALIGNED NO-LABEL
     btFechaAviso AT ROW 4.5 COL 59.14
     btLimpa AT ROW 4.5 COL 69.86
     vLigado AT ROW 4.75 COL 25.72
     vDesStatusEmbarque AT ROW 5.5 COL 83.57 NO-LABEL
     vIntervalo AT ROW 5.67 COL 23.72 COLON-ALIGNED
     c-hora AT ROW 7.38 COL 82 COLON-ALIGNED NO-LABEL
     bresumo AT ROW 7.42 COL 2
     btlistaitem AT ROW 9.17 COL 90
     btLegenda AT ROW 10.83 COL 90
     btlimpaitem AT ROW 12.54 COL 90
     bConfere AT ROW 12.75 COL 2
     bt-relat-etiquetas AT ROW 14.29 COL 90 WIDGET-ID 2
     de-qt-volume AT ROW 16.96 COL 91.72 COLON-ALIGNED
     de-qt-peso AT ROW 18.21 COL 91.72 COLON-ALIGNED
     de-qt-metro AT ROW 19.46 COL 91.72 COLON-ALIGNED
     "Fechar/Reiniciar Embarque" VIEW-AS TEXT
          SIZE 20 BY .54 AT ROW 4 COL 57.29
     "Situá∆o Embarque" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 3.08 COL 84.14
     "Totais Separados do Resumo" VIEW-AS TEXT
          SIZE 20 BY .54 AT ROW 16.25 COL 85
     "Imprime Etiquetas" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 1.75 COL 58
     "de embalagens:" VIEW-AS TEXT
          SIZE 12 BY .75 AT ROW 2.5 COL 58
     "Situá∆o Resumo" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 4.92 COL 84.29
     RECT-1 AT ROW 1.5 COL 1.14
     RECT-10 AT ROW 16.46 COL 83.57
     RECT-11 AT ROW 3.42 COL 82.86
     RECT-12 AT ROW 5.25 COL 83
     RECT-13 AT ROW 1.5 COL 82.86
     RECT-14 AT ROW 7 COL 83
     RECT-2 AT ROW 12.42 COL 1
     RECT-4 AT ROW 7 COL 1
     RECT-6 AT ROW 4.25 COL 54.86
     RECT-8 AT ROW 9 COL 83
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.57 BY 20.54
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wgEmbarque ASSIGN
         HIDDEN             = YES
         TITLE              = "Conferància do embarque"
         HEIGHT             = 20.13
         WIDTH              = 107.72
         MAX-HEIGHT         = 28.21
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.21
         VIRTUAL-WIDTH      = 146.29
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-wgEmbarque:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wgEmbarque 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wgEmbarque
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fEmbarque
   FRAME-NAME                                                           */
/* BROWSE-TAB bresumo c-hora fEmbarque */
/* BROWSE-TAB bConfere btlimpaitem fEmbarque */
/* SETTINGS FOR BUTTON btFechaAviso IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN c-sit-embarque IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-qt-metro IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-qt-peso IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN de-qt-volume IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vDesStatusEmbarque IN FRAME fEmbarque
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN vIntervalo IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vnrresumo-fim IN FRAME fEmbarque
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vnrresumo-ini IN FRAME fEmbarque
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wgEmbarque)
THEN wgEmbarque:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bConfere
/* Query rebuild information for BROWSE bConfere
     _START_FREEFORM
OPEN QUERY bConfere FOR EACH tt-confere-aviso,
                        FIRST saldo-estoq NO-LOCK
                        WHERE saldo-estoq.it-codigo = tt-confere-aviso.cod-item
                          AND saldo-estoq.cod-refer = tt-confere-aviso.cod-refer
                          /*AND saldo-estoq.cod-depos = "ARM" */ .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bConfere */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bresumo
/* Query rebuild information for BROWSE bresumo
     _START_FREEFORM
OPEN QUERY bresumo FOR EACH tt-pre-fatur.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bresumo */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fEmbarque:HANDLE
       ROW             = 2
       COLUMN          = 4
       HEIGHT          = 1.17
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(btConfere:HANDLE IN FRAME fEmbarque).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wgEmbarque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wgEmbarque wgEmbarque
ON END-ERROR OF wgEmbarque /* Conferància do embarque */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wgEmbarque wgEmbarque
ON WINDOW-CLOSE OF wgEmbarque /* Conferància do embarque */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bConfere
&Scoped-define SELF-NAME bConfere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bConfere wgEmbarque
ON ROW-DISPLAY OF bConfere IN FRAME fEmbarque
DO:
    If   tt-confere-aviso.qtd-embarcada = 0 Then Do:
         Assign tt-confere-aviso.cod-item     :Bgcolor In Browse bConfere = 12
                tt-confere-aviso.COD-REFER    :Bgcolor In Browse bConfere = 12
                tt-confere-aviso.des-item     :Bgcolor In Browse bConfere = 12
                tt-confere-aviso.qtd-faturada :Bgcolor In Browse bConfere = 12
                tt-confere-aviso.qtd-embarcada:Bgcolor In Browse bConfere = 12
                tt-confere-aviso.qtd-aberto   :BGCOLOR In Browse bConfere = 12
                saldo-estoq.qtidade-atu       :BGCOLOR In Browse bConfere = 12.
    End.
    Else 
    If   tt-confere-aviso.qtd-faturada   > tt-confere-aviso.qtd-embarcada And
         tt-confere-aviso.qtd-embarcada <> 0 Then Do:
         Assign tt-confere-aviso.cod-item     :Bgcolor In Browse bConfere = 14 
                tt-confere-aviso.COD-REFER    :Bgcolor In Browse bConfere = 14
                tt-confere-aviso.des-item     :Bgcolor In Browse bConfere = 14
                tt-confere-aviso.qtd-faturada :Bgcolor In Browse bConfere = 14
                tt-confere-aviso.qtd-embarcada:Bgcolor In Browse bConfere = 14
                tt-confere-aviso.qtd-aberto   :Bgcolor In Browse bConfere = 14
                saldo-estoq.qtidade-atu       :BGCOLOR In Browse bConfere = 14.
    End.
    Else
    If   tt-confere-aviso.qtd-faturada   = tt-confere-aviso.qtd-embarcada Then Do:
         Assign tt-confere-aviso.cod-item     :Bgcolor In Browse bConfere = 2
                tt-confere-aviso.COD-REFER    :Bgcolor In Browse bConfere = 2
                tt-confere-aviso.des-item     :Bgcolor In Browse bConfere = 2
                tt-confere-aviso.qtd-faturada :Bgcolor In Browse bConfere = 2
                tt-confere-aviso.qtd-embarcada:Bgcolor In Browse bConfere = 2
                tt-confere-aviso.qtd-aberto   :Bgcolor In Browse bConfere = 2
                saldo-estoq.qtidade-atu       :BGCOLOR In Browse bConfere = 2.
    End.
    Else                
    If   tt-confere-aviso.qtd-faturada   < tt-confere-aviso.qtd-embarcada Then Do:
         Assign tt-confere-aviso.cod-item     :Bgcolor In Browse bConfere = 13   
                tt-confere-aviso.COD-REFER    :Bgcolor In Browse bConfere = 13
                tt-confere-aviso.des-item     :Bgcolor In Browse bConfere = 13  
                tt-confere-aviso.qtd-faturada :BGCOLOR In Browse bConfere = 13    
                tt-confere-aviso.qtd-embarcada:Bgcolor In Browse bConfere = 13
                tt-confere-aviso.qtd-aberto   :Bgcolor In Browse bConfere = 13
                saldo-estoq.qtidade-atu       :BGCOLOR In Browse bConfere = 13.
    End.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bresumo
&Scoped-define SELF-NAME bresumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bresumo wgEmbarque
ON ITERATION-CHANGED OF bresumo IN FRAME fEmbarque
DO:
  
  if not avail tt-pre-fatur 
    then 
        return no-apply.
        
  assign vNumAviso     = tt-pre-fatur.cdd-embarq
         vnrresumo     = tt-pre-fatur.nr-resumo
         l-mostra-item = yes.
  
  Run piConfereEmbarque.
  
  /*********  
  assign tt-pre-fatur.nr-embarque  :bgcolor In Browse bresumo = i-sit-resumo
         tt-pre-fatur.nr-resumo    :bgcolor In Browse bresumo = i-sit-resumo
         tt-pre-fatur.dt-embarque  :bgcolor In Browse bresumo = i-sit-resumo
         tt-pre-fatur.nome-abrev   :bgcolor In Browse bresumo = i-sit-resumo
         tt-pre-fatur.motorista    :bgcolor In Browse bresumo = i-sit-resumo
         tt-pre-fatur.placa        :bgcolor In Browse bresumo = i-sit-resumo
         tt-pre-fatur.uf-placa     :bgcolor In Browse bresumo = i-sit-resumo
         vUsuar                    :bgcolor In Browse bresumo = i-sit-resumo.
  *******/
  
  
  assign l-mostra-item = no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bresumo wgEmbarque
ON MOUSE-SELECT-CLICK OF bresumo IN FRAME fEmbarque
DO:
        
  if not avail tt-pre-fatur 
    then 
        return no-apply.
        
  assign vNumAviso     = tt-pre-fatur.cdd-embarq
         vnrresumo     = tt-pre-fatur.nr-resumo
         l-mostra-item = yes.
  
  Run piConfereEmbarque.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bresumo wgEmbarque
ON MOUSE-SELECT-DBLCLICK OF bresumo IN FRAME fEmbarque
DO:
        
  if not avail tt-pre-fatur 
    then 
        return no-apply.
        
  assign vNumAviso     = tt-pre-fatur.cdd-embarq
         vnrresumo     = tt-pre-fatur.nr-resumo
         l-mostra-item = yes.
  
  Run piConfereEmbarque.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bresumo wgEmbarque
ON ROW-DISPLAY OF bresumo IN FRAME fEmbarque
DO:
&IF  '{&mgcld_version}' >= '2.04'
&THEN
   if not avail tt-pre-fatur then return no-apply.
   
   Find First bc-etiqueta no-lock
         Where bc-etiqueta.nr-embarque = INT(tt-pre-fatur.cdd-embarq)
         and   bc-etiqueta.nr-resumo    = tt-pre-fatur.nr-resumo 
         No-Error.

   Assign vUsuar = (If Avail bc-etiqueta 
                    Then bc-etiqueta.usuar-separacao 
                    Else "").

&ENDIF 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-relat-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-relat-etiquetas wgEmbarque
ON CHOOSE OF bt-relat-etiquetas IN FRAME fEmbarque /* Salvar Etiq */
DO:
  if bconfere:num-selected-rows in frame {&FRAME-NAME} = 0 
  THEN DO:
      MESSAGE "Selecione os itens que deseja salvar"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      Return no-apply. 
  END.
  
  
  ASSIGN arq-etq-retiradas-dtl = SESSION:TEMP-DIRECTORY + "etq-" + vNumAviso:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ".csv"
         arq-etq-retiradas-smp = SESSION:TEMP-DIRECTORY + "etq-" + vNumAviso:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ".txt"
         var-qt-etq-sel        = 0.
 
  RUN pi-imp-header-etq.

  DO i-cont = 1 to bconfere:num-selected-rows in frame {&FRAME-NAME}:
     if bconfere:fetch-selected-row(i-cont) in frame {&FRAME-NAME} then do trans:
     
        FIND CURRENT tt-confere-aviso no-lock no-error.

        FOR EACH bc-etiqueta WHERE bc-etiqueta.nr-embarque  = tt-confere-aviso.nr-embarque
                               AND bc-etiqueta.nr-resumo    = tt-confere-aviso.nr-resumo
                               AND bc-etiqueta.it-codigo    = tt-confere-aviso.cod-item
                               AND bc-etiqueta.referencia   = tt-confere-aviso.cod-refer
                             NO-LOCK:
             ASSIGN var-qt-etq-sel = var-qt-etq-sel + 1.
             RUN pi-imp-dados-etq.
         END.
     END.
  END.

  RUN pi-imp-fecha-etq.
  
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-SAIR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-SAIR wgEmbarque
ON CHOOSE OF BT-SAIR IN FRAME fEmbarque /* SAIR */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConfere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfere wgEmbarque
ON CHOOSE OF btConfere IN FRAME fEmbarque /* Confere */
DO:
    
    Assign Input Frame {&Frame-Name} vNumAviso.
    Assign Input Frame {&Frame-Name} vNumAvisofim.
    Assign Input Frame {&Frame-Name} vnrresumo-ini.
    Assign Input Frame {&Frame-Name} vnrresumo-fim.
        
    If  btFechaAviso:Load-Image('image/im-smate.bmp') Then.
    
    Close Query bresumo.
    Close Query bconfere.
    
    Assign vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = 'Em processamento...'
           chTick:enabled  = vligado.
                 
    For each tt-pre-fatur:
        Delete tt-pre-fatur.
    End.
    
    run utp/ut-acomp.p persistent set h-acomp.
    
    run pi-inicializar in h-acomp (input  "Leitura de Embarques").
    run pi-acompanhar in h-acomp (input "Lendo Embarques...").

    
    for each tt-integracao-401:
        delete tt-integracao-401.
    end.
    
    create tt-integracao-401.
    assign 
           tt-integracao-401.cdd-embarq-ini = vnumaviso
           tt-integracao-401.cdd-embarq-fim = vnumavisofim
           tt-integracao-401.nr-resumo-ini = vnrresumo-ini 
           tt-integracao-401.nr-resumo-fim = vnrresumo-fim
           tt-integracao-401.cod-versao-integracao = 1
           l-mostra-item = no.
    
    run ftp/ftapi401.p (input  table tt-integracao-401,
                        output table tt-pre-fatur,
                        input-output table tt-erro).
    
    
    
    Find First tt-pre-fatur No-error.
    
    If  Not Avail tt-pre-fatur Then Do:
        
        run pi-finalizar in h-acomp.
        
        Run utp/ut-msgs.p  (Input "show",
                            Input 19558,
                            Input '').

        Disable btFechaAviso With Frame {&Frame-Name}.
        Return No-apply.
    End. /* If  Not Avail tt */
    
    run pi-acompanhar in h-acomp (input "Tratando Embarques...").

    for each tt-pre-fatur:
        run pi-acompanhar in h-acomp (input "Embarque: " + string(tt-pre-fatur.cdd-embarq)).
        
        find b-tt-pre-fatur
             where  rowid(b-tt-pre-fatur)     <> rowid(tt-pre-fatur)
             and    b-tt-pre-fatur.nr-resumo  = tt-pre-fatur.nr-resumo
             and    b-tt-pre-fatur.cdd-embarq = tt-pre-fatur.cdd-embarq
             no-lock no-error.
             
        if  avail b-tt-pre-fatur 
        then 
            delete tt-pre-fatur.          
    end.
    
    run pi-acompanhar in h-acomp (input "Listando Embarques...").
        
    OPEN QUERY bresumo FOR EACH tt-pre-fatur.
    
    run pi-finalizar in h-acomp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEtqEmb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEtqEmb wgEmbarque
ON CHOOSE OF btEtqEmb IN FRAME fEmbarque /* Imprime Etq Emb */
DO:    
&IF  '{&mgcld_version}' >= '2.04'
&THEN
    Define Variable vLogFechado         As Logical          No-undo.

    Find First bc-etiqueta no-lock
         Where bc-etiqueta.nr-embarque   = vNumAviso
         and   bc-etiqueta.nr-resumo   = vnrresumo         
         No-Error.
  
    If  not avail bc-etiqueta Then
    DO:
        RUN MESSAGE.p (INPUT 'Etiquetas ainda n∆o romaneadas', 
                           'N∆o foram encontradas etiquetas para este embarque.').
        Return no-apply.

    END.
               
    RUN bcp/bcx003etq.w (INPUT vNumAviso,
                         INPUT vNrresumo).

&ENDIF    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFechaAviso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFechaAviso wgEmbarque
ON CHOOSE OF btFechaAviso IN FRAME fEmbarque /* Fecha */
DO:
&IF  '{&mgcld_version}' >= '2.04'
&THEN
   
   .
   
   
   If  btFechaAviso:Load-Image('image/im-smate.bmp') Then.

   Find First bc-etiqueta no-lock
        Where bc-etiqueta.nr-embarque = vNumAviso 
        and   bc-etiqueta.nr-resumo   = vnrresumo 
        No-Error.
  
   If  Not AvailAble bc-etiqueta Then Do:
      Run utp/ut-msgs.p  (Input "show",
                          Input 19562,
                          Input '').
      return no-apply.
   End.
         
   FIND RES-CLI
        WHERE RES-CLI.NR-EMBARQUE = VNUMAVISO
        AND   RES-CLI.NR-RESUMO   = VNRRESUMO
        NO-LOCK NO-ERROR.

   IF  AVAIL RES-CLI
   AND RES-CLI.SITUACAO > 1 /* ALOCADO */
   THEN DO:

       MESSAGE "EMBARQUE Jµ FOI CALCULADO !!!" SKIP
               "EMBARQUE N«O PODE SER REABERTO" 
               VIEW-AS ALERT-BOX. 

       RETURN NO-APPLY.

   END.

   Run 'btb/btb910zz.p' (Input v_cod_usuar_corren,
                          Input Yes).
                          
   If  Return-Value = 'NOK' Then Do:
       Return No-apply.
   End.
            
   If  bc-etiqueta.cod-estado = 4 
   Then Do:
       
       Assign vLogFechado = Yes.
       Run utp/ut-msgs.p  (Input "show",
                           Input 19557,
                           Input '').
   End.
   Else Do:
   
       Assign vLogFechado = No.
       Run utp/ut-msgs.p  (Input "show",
                           Input 19556,
                           Input '').
   End.
         
   If  Return-Value <> 'Yes' 
   Then 
       return no-apply.

   RUN bcp/bcx1000.p PERSISTENT SET h_bcx1000.

   RUN PI-ITEM-ATENDIDO IN h_bcx1000 (vnumaviso,
                                      vnrresumo,
                                      vnrresumo,
                                      "", 
                                      "ZZZZZZZZZZZZZZZZZZZZ", 
                                      "",
                                      "ZZZZZZZZZZZZZZZZZZZZ",
                                      OUTPUT C-ITEM-FALTA, 
                                      OUTPUT DE-QT-TOTAL,
                                      OUTPUT DE-QT-LIDO).

  
   DELETE OBJECT h_bcx1000.
   
   if  RETURN-VALUE <> "OK" /* separado */ 
   AND vLogFechado = No
   then do:
   
       for each  tt-pre-fatur
           where tt-pre-fatur.nr-resumo   = vnrresumo
           and   tt-pre-fatur.cdd-embarq = vnumaviso
           break by tt-pre-fatur.cdd-embarq:     
        
           Run utp/ut-msgs.p  (Input "show",
                               Input 19561,
                               Input '').
                                 
           If  Return-Value <> 'Yes' Then 
               return no-apply.
          
       end.

   end.                 

   ASSIGN l-erro-emb = NO.
   DO TRANS:
                  
       FOR EACH tt-it-pre-fat-Bo:
           DELETE tt-it-pre-fat-Bo.
       END.

       FOR EACH it-pre-fat
           WHERE it-pre-fat.nr-embarque = vNumAviso
           AND   it-pre-fat.nr-resumo   = vnrresumo 
           NO-LOCK:

           CREATE tt-it-pre-fat-Bo.
           ASSIGN tt-it-pre-fat-Bo.it-codigo    = it-pre-fat.it-codigo
                  tt-it-pre-fat-Bo.cod-refer    = it-pre-fat.cod-refer
                  tt-it-pre-fat-Bo.nr-embarque  = it-pre-fat.nr-embarque
                  tt-it-pre-fat-Bo.nr-resumo    = it-pre-fat.nr-resumo
                  tt-it-pre-fat-Bo.nr-entrega   = it-pre-fat.nr-entrega
                  tt-it-pre-fat-Bo.nome-abrev   = it-pre-fat.nome-abrev
                  tt-it-pre-fat-Bo.nr-pedcli    = it-pre-fat.nr-pedcli
                  tt-it-pre-fat-Bo.nr-sequencia = it-pre-fat.nr-sequencia
                  tt-it-pre-fat-Bo.qt-coletada  = 0.

       END.

       For each  b-bc-etiqueta 
           Where b-bc-etiqueta.nr-embarque = vNumAviso
           and   b-bc-etiqueta.nr-resumo   = vnrresumo
           NO-LOCK:
           
           FIND bc-etiqueta
                WHERE ROWID(bc-etiqueta) = ROWID(b-bc-etiqueta)
                share-LOCK NO-ERROR.

           Assign bc-etiqueta.cod-estado      = If vLogFechado = YES 
                                                Then 3 /* SEPARADO */
                                                Else 4 /* EXPEDIDO */.
           
           FIND tt-it-pre-fat-Bo
                WHERE tt-it-pre-fat-Bo.it-codigo    = bc-etiqueta.it-codigo 
                  AND tt-it-pre-fat-Bo.cod-refer    = bc-etiqueta.referencia 
                  AND tt-it-pre-fat-Bo.nr-embarque  = bc-etiqueta.nr-embarque
                  AND tt-it-pre-fat-Bo.nr-resumo    = bc-etiqueta.nr-resumo  
                  AND tt-it-pre-fat-Bo.nr-entrega   = bc-etiqueta.nr-entrega 
                  AND tt-it-pre-fat-Bo.nome-abrev   = bc-etiqueta.nome-abrev 
                  AND tt-it-pre-fat-Bo.nr-pedcli    = bc-etiqueta.nr-pedcli  
                  AND tt-it-pre-fat-Bo.nr-sequencia = bc-etiqueta.nr-seq-fat 
                NO-LOCK NO-ERROR.

           IF NOT AVAIL tt-it-pre-fat-bo THEN DO:

              CREATE tt-it-pre-fat-Bo.
              ASSIGN tt-it-pre-fat-Bo.it-codigo    = bc-etiqueta.it-codigo
                     tt-it-pre-fat-Bo.cod-refer    = bc-etiqueta.referencia 
                     tt-it-pre-fat-Bo.nr-embarque  = bc-etiqueta.nr-embarque
                     tt-it-pre-fat-Bo.nr-resumo    = bc-etiqueta.nr-resumo
                     tt-it-pre-fat-Bo.nr-entrega   = bc-etiqueta.nr-entrega
                     tt-it-pre-fat-Bo.nome-abrev   = bc-etiqueta.nome-abrev
                     tt-it-pre-fat-Bo.nr-pedcli    = bc-etiqueta.nr-pedcli
                     tt-it-pre-fat-Bo.nr-sequencia = bc-etiqueta.nr-seq-fat.
           END.
                  
           ASSIGN tt-it-pre-fat-Bo.qt-coletada = tt-it-pre-fat-bo.qt-coletada + bc-etiqueta.qt-item.
           
       End. /* each bc-etiqueta */
                    

       IF  NOT vLogFechado THEN do:
           RUN imp/imft001.p (INPUT TABLE tt-it-pre-fat-Bo, INPUT YES, OUTPUT TABLE tt-erro).       

           RUN esapi/act-embarque.p (INPUT tt-it-pre-fat-bo.nr-embarque).
       END.

       FIND FIRST tt-erro NO-LOCK NO-ERROR.
       IF  AVAIL tt-erro OR RETURN-VALUE = "NO" 
       THEN DO:
            ASSIGN l-erro-emb = YES.
            ASSIGN c-mess-erro = "Atená∆o ! Problemas no embarque: " + string(vNumAviso) + " resumo: " + STRING(vnrresumo) + " !".
            RUN MESSAGE.p (INPUT c-mess-erro,
                           INPUT "N∆o foi poss°vel alterar a quantidade do embarque. Verifique se o " +
                                "Cliente tem permiss∆o para alocar quantidades maiores que o pedido ou " +
                                "se existe estoque suficiente, sendo que nesse caso pode ser que o" +
                                "produto j† tenha sido alocado para outro pedido.").
            UNDO, LEAVE.
       END.
   END.
   
   IF l-erro-emb = NO
   THEN DO:
        RUN pi-altera-pedido.
        Apply 'Choose' To btConfere In Frame {&Frame-Name}.
   END.
   RETURN NO-APPLY.

&ENDIF    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLegenda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLegenda wgEmbarque
ON CHOOSE OF btLegenda IN FRAME fEmbarque /* Legenda */
DO:
  Run 'bcp/bc9008g.w'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLimpa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLimpa wgEmbarque
ON CHOOSE OF btLimpa IN FRAME fEmbarque /* Limpa */
DO:
&IF  '{&mgcld_version}' >= '2.04'
&THEN
    Define Variable vLogFechado         As Logical          No-undo.

    Find First bc-etiqueta no-lock
         Where bc-etiqueta.nr-embarque   = vNumAviso
         and   bc-etiqueta.nr-resumo   = vnrresumo         
         No-Error.
  
    If  not avail bc-etiqueta Then
        Return no-apply.
        
    Assign vLogFechado = IF bc-etiqueta.COD-ESTADO = 4 /* EXPEDIDO */ 
                         THEN YES 
                         ELSE NO.
    
    If  vLogFechado Then Do:
        Run utp/ut-msgs.p  (Input "show",
                            Input 19622,
                            Input '').
        Return No-Apply.
    End.

    Run utp/ut-msgs.p  (Input "show",
                        Input 19620,
                        Input '').
                        
    If  Return-Value <> 'Yes' Then Do:
        Return No-Apply.
    End. /* Return-Value <> 'Yes' */
    
    Assign Input Frame {&Frame-Name} vNumAviso.

    If  btFechaAviso:Load-Image('image/im-smate.bmp') Then.
  
    DO TRANS:
        
        For each b-bc-etiqueta 
            Where b-bc-etiqueta.nr-embarque = vNumAviso
            and   b-bc-etiqueta.nr-resumo   = vnrresumo       
            NO-LOCK:
            
            FIND bc-etiqueta
                 WHERE ROWID(bc-etiqueta) = ROWID(b-bc-etiqueta)
                 share-LOCK NO-ERROR.

            RUN PI-LIBERA-ETIQUETA.
           
        End. /* each bc-etiqueta */
    END.

    Apply 'Choose' To btConfere In Frame {&Frame-Name}.

&ENDIF    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btlimpaitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btlimpaitem wgEmbarque
ON CHOOSE OF btlimpaitem IN FRAME fEmbarque /* Retirar Item */
DO:
&IF  '{&mgcld_version}' >= '2.04'
&THEN
  
  if bconfere:num-selected-rows in frame {&FRAME-NAME} = 0 
  then
      Return no-apply. 
  
  Find First bc-etiqueta no-lock
       Where bc-etiqueta.nr-embarque = vNumAviso 
       and   bc-etiqueta.nr-resumo   = vnrresumo
       
       No-Error.

  If  not avail bc-etiqueta Then
      Return no-apply.
  
  If  BC-ETIQUETA.COD-ESTADO = 4
  Then Do:
      Run utp/ut-msgs.p  (Input "show",
                          Input 19622,
                          Input '').
      Return No-Apply.
  End.
  
 ASSIGN arq-etq-retiradas-dtl = SESSION:TEMP-DIRECTORY + "etq-retiradas-" + vNumAviso:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ".csv"
        arq-etq-retiradas-smp = SESSION:TEMP-DIRECTORY + "etq-retiradas-" + vNumAviso:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ".txt"
        var-qt-etq-sel        = 0.
 
 
 
 RUN pi-imp-header-etq.
   
 DO i-cont = 1 to bconfere:num-selected-rows in frame {&FRAME-NAME}:
  
     if  bconfere:fetch-selected-row(i-cont) in frame {&FRAME-NAME} then do trans:
     
         find current tt-confere-aviso no-lock no-error.

         FOR  EACH  b-bc-etiqueta 
              where b-bc-etiqueta.nr-embarque  = tt-confere-aviso.nr-embarque
              and   b-bc-etiqueta.nr-resumo    = tt-confere-aviso.nr-resumo
              and   b-bc-etiqueta.it-codigo    = tt-confere-aviso.cod-item
              and   b-bc-etiqueta.referencia   = tt-confere-aviso.cod-refer                     
              NO-LOCK:

             FIND bc-etiqueta
                  WHERE ROWID(bc-etiqueta) = ROWID(b-bc-etiqueta)
                  share-LOCK NO-ERROR.
             
             RUN pi-imp-dados-etq.
             RUN PI-LIBERA-ETIQUETA.
             ASSIGN var-qt-etq-sel = var-qt-etq-sel + 1.

         END.
         
     end.
     
  end.
  
  RUN pi-imp-fecha-etq.
  
  APPLY "CHOOSE" TO btlistaitem IN FRAME {&FRAME-NAME}.

&ENDIF    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btlistaitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btlistaitem wgEmbarque
ON CHOOSE OF btlistaitem IN FRAME fEmbarque /* Lista item */
DO:
  assign l-mostra-item = yes.
  run piConfereEmbarque.
  assign l-mostra-item = no.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Conferir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Conferir wgEmbarque
ON CHOOSE OF MENU-ITEM m_Conferir /* Conferir */
DO:
  Apply 'Choose' To btConfere In Frame {&Frame-Name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sair wgEmbarque
ON CHOOSE OF MENU-ITEM m_Sair /* Sair */
DO:
  Apply 'Close' To wgEmbarque.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vIntervalo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vIntervalo wgEmbarque
ON LEAVE OF vIntervalo IN FRAME fEmbarque /* Tempo Atualizaá∆o (segs) */
DO:
    Assign Input Frame {&Frame-Name} vIntervalo.
    
    Assign chTick:Interval = (vIntervalo * 1000)
           chTick:enabled  = yes.
           
    run CtrlFrame.PSTimer.Tick.   
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vLigado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vLigado wgEmbarque
ON VALUE-CHANGED OF vLigado IN FRAME fEmbarque /* Atualizar Automaticamente */
DO:
  Assign Input Frame {&Frame-Name} vLigado
         Input Frame {&Frame-Name} vIntervalo.
  
  Case vLigado:
    When Yes 
    Then do:
           Enable  vIntervalo With Frame {&Frame-Name}.
           ASSIGN chTick:Interval = (vIntervalo * 1000).
           assign chTick:enabled = yes.           
         end.     
    When No  
    Then do:
           Disable vIntervalo With Frame {&Frame-Name}.
           assign chTick:enabled = no.
         end.
  End. /* Case */
  
  run CtrlFrame.PSTimer.Tick.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vnrresumo-fim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vnrresumo-fim wgEmbarque
ON RETURN OF vnrresumo-fim IN FRAME fEmbarque
DO:
  Apply 'choose' To btConfere In Frame {&Frame-Name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vnrresumo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vnrresumo-ini wgEmbarque
ON RETURN OF vnrresumo-ini IN FRAME fEmbarque /* N£mero Resumo */
DO:
  Apply 'choose' To btConfere In Frame {&Frame-Name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vNumAviso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vNumAviso wgEmbarque
ON LEAVE OF vNumAviso IN FRAME fEmbarque /* Embarque */
DO:
 ASSIGN vnumavisofim  = INPUT vnumaviso.
 DISP vnumavisofim WITH FRAME {&FRAME-NAME}.
 APPLY 'choose' TO btconfere.
 ASSIGN vligado = YES.
 DISP vligado WITH FRAME {&FRAME-NAME}.
 APPLY 'mouse-select-dblclick' TO bresumo. 
 ASSIGN vligado = NO.
 DISP vligado WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vNumAviso wgEmbarque
ON RETURN OF vNumAviso IN FRAME fEmbarque /* Embarque */
DO:
  Apply 'choose' To btConfere In Frame {&Frame-Name}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vNumAvisofim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vNumAvisofim wgEmbarque
ON RETURN OF vNumAvisofim IN FRAME fEmbarque
DO:
  
  Apply 'choose' To btConfere In Frame {&Frame-Name}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vtresumo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vtresumo wgEmbarque
ON VALUE-CHANGED OF vtresumo IN FRAME fEmbarque /* Todos os Resumos */
DO:
  Assign Input Frame {&Frame-Name} vtresumo.
  Case vtresumo:
    When Yes 
    Then do:
           Disable vnrresumo-ini With Frame {&Frame-Name}.
           Disable vnrresumo-fim With Frame {&Frame-Name}.           
         end.     
    When No  
    Then do:
           Enable vnrresumo-ini With Frame {&Frame-Name}.
           Enable vnrresumo-fim With Frame {&Frame-Name}.           
         end.
  End. /* Case */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bConfere
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wgEmbarque 


/* ***************************  Main Block  *************************** */

/*{esinc/i-acesso.i "bcx0003f"}*/

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wgEmbarque  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available wgEmbarque  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wgEmbarque  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "bcx0003f.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "bcx0003f.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlFrame.PSTimer.Tick wgEmbarque 
PROCEDURE CtrlFrame.PSTimer.Tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  apply "CHOOSE":U to btlistaitem in frame {&FRAME-NAME}.

  ASSIGN c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(TODAY,"99/99/9999") 
                                                      + " - " +
                                                      STRING(TIME,"HH:MM:SS").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wgEmbarque  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wgEmbarque)
  THEN DELETE WIDGET wgEmbarque.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wgEmbarque  _DEFAULT-ENABLE
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
  DISPLAY vNumAviso vNumAvisofim vtresumo c-sit-embarque vnrresumo-ini 
          vnrresumo-fim vLigado vDesStatusEmbarque vIntervalo c-hora 
          de-qt-volume de-qt-peso de-qt-metro 
      WITH FRAME fEmbarque IN WINDOW wgEmbarque.
  ENABLE RECT-1 RECT-10 RECT-11 RECT-12 RECT-13 RECT-14 RECT-2 RECT-4 RECT-6 
         RECT-8 BT-SAIR btEtqEmb vNumAviso vNumAvisofim btConfere vtresumo 
         btLimpa vLigado c-hora bresumo btlistaitem btLegenda btlimpaitem 
         bConfere bt-relat-etiquetas 
      WITH FRAME fEmbarque IN WINDOW wgEmbarque.
  {&OPEN-BROWSERS-IN-QUERY-fEmbarque}
  VIEW wgEmbarque.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit wgEmbarque 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize wgEmbarque 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/


&IF  '{&mgcld_version}' >= '2.04'
&THEN

  If  v_cod_usuar_corren = '' Then 
      Run 'btb/btb910za.p'.

  /* Code placed here will execute PRIOR to standard behavior. */


{utp/ut9000.i "BCX0003F" "2.00.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .


  Assign chTick = chCtrlFrame:Controls:Item(1).

  /* Code placed here will execute AFTER standard behavior.    */

{utp/ut-liter.i "Embarque" * L}
assign tt-pre-fatur.cdd-embarq:label in browse bresumo = trim(return-value).

{utp/ut-liter.i "Resumo" * L}
assign tt-pre-fatur.nr-resumo:label in browse bresumo = trim(return-value).

{utp/ut-liter.i "Dt Embar" * L}
assign tt-pre-fatur.dt-embarque:label in browse bresumo = return-value.

{utp/ut-liter.i "Motorista" * L}
assign tt-pre-fatur.motorista:label in browse bresumo = return-value.

{utp/ut-liter.i "Placa" * L}
assign tt-pre-fatur.placa:label in browse bresumo = return-value.

{utp/ut-liter.i "UF" * L}
assign tt-pre-fatur.uf-placa:label in browse bresumo = return-value.

{utp/ut-liter.i "Cliente" * L}
assign tt-pre-fatur.nome-abrev:label in browse bresumo = return-value.

{utp/ut-liter.i "Usu†rio" * L}
assign vUsuar:label in browse bresumo = return-value.

{utp/ut-liter.i "Atualizando" * L}
assign c-atualizando = return-value.

{utp/ut-liter.i "Ultima Atulizaá∆o" * L}
assign c-ultima-atualizacao = return-value.

{utp/ut-liter.i "Resumo do Embarque Liberado" * L}
assign c-fechado = return-value.

{utp/ut-liter.i "Resumo do Embarque n∆o Liberado" * L}
assign c-aberto = return-value.

{utp/ut-liter.i "Fechar Resumo do Embarque" * L}
assign c-fechar = return-value.

{utp/ut-liter.i "Abrir  Resumo do Embarque" * L}
assign c-abrir = return-value.

Find First bc-prog-trans no-lock 
     where bc-prog-trans.cod_prog_dtsul = 'bc9008' No-error.

If  Not AvailAble bc-prog-trans Then Do:

    message 'Progama bc9008 n∆o esta relacionado com nemhuma transaá∆o'. 
    
    return no-apply.
    
End.
assign c-transacao = bc-prog-trans.cd-trans.

assign  vDesStatusEmbarque:bgcolor In Frame {&Frame-Name}       = 12
         vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = "Resumo Aberto...". 

&ELSE
    run utp/ut-msgs.p ( input "SHOW",
                        input 25505,
                        input "").

    RUN dispatch IN THIS-PROCEDURE ('destroy':U).

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera-pedido wgEmbarque 
PROCEDURE pi-altera-pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FIND FIRST pre-fatur WHERE pre-fatur.nr-embarque = VNUMAVISO NO-LOCK NO-ERROR.
   IF AVAIL pre-fatur THEN DO.
      FIND FIRST ped-venda WHERE ped-venda.nr-pedcli   = pre-fatur.nr-pedcli SHARE-LOCK NO-ERROR.
      IF AVAIL ped-venda THEN 
         ASSIGN ped-venda.dsp-pre-fat = NO. /*Passo o pedido para n∆o disponivel para o pre-faturamento*/
          /*ASSIGN ped-venda.cod-sit-pre   = IF vLogFechado = NO THEN 3 ELSE 1. /* Passa situacao do pedido para alocado total  -- Anderson Fagner 20/10/2010 */*/
   END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-dados-etq wgEmbarque 
PROCEDURE pi-imp-dados-etq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
PUT STREAM eqt-dtl UNFORMAT bc-etiqueta.progressivo       ";"
                                bc-etiqueta.it-codigo         ";"
                                bc-etiqueta.referencia        ";"
                                bc-etiqueta.qt-item           ";"
                                BC-ETIQUETA.nr-embarque       ";"
                                BC-ETIQUETA.NR-RESUMO         ";"
                                BC-ETIQUETA.NR-SEQ-FAT        ";"
                                BC-ETIQUETA.NOME-ABREV        ";"
                                BC-ETIQUETA.NR-PEDCLI         ";"
                                BC-ETIQUETA.NR-ENTREGA        ";"
                                BC-ETIQUETA.COD-ESTADO        ";"
                                BC-ETIQUETA.DT-SEPARACAO      ";"
                                BC-ETIQUETA.HR-SEPARACAO      ";"
                                BC-ETIQUETA.usuar-separacao   ";"
                                SKIP.
    
PUT STREAM eqt-smp UNFORMAT TRIM(bc-etiqueta.progressivo) SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-fecha-etq wgEmbarque 
PROCEDURE pi-imp-fecha-etq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM eqt-dtl CLOSE.
OUTPUT STREAM eqt-smp CLOSE.
IF var-qt-etq-sel <> 0 THEN DO.
   MESSAGE "Foram encontradas " var-qt-etq-sel " etiquetas." SKIP
           "O Relatorio foi salvo em: " arq-etq-retiradas-dtl SKIP
           "Deseja abrir?"
           VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE var-abrir-relat-etq.
   IF var-abrir-relat-etq THEN DO:
      /*OS-COMMAND NO-WAIT VALUE(arq-etq-retiradas-dtl).*/
      OS-COMMAND NO-WAIT VALUE(arq-etq-retiradas-smp).
   END.
   ASSIGN var-qt-etq-sel = 0.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-header-etq wgEmbarque 
PROCEDURE pi-imp-header-etq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT STREAM eqt-dtl TO value(arq-etq-retiradas-dtl).
OUTPUT STREAM eqt-smp TO value(arq-etq-retiradas-smp).
    
    PUT STREAM eqt-dtl "progressivo    ;"
                       "it-codigo      ;"
                       "referencia     ;"
                       "qt-item        ;"
                       "nr-embarque    ;"
                       "NR-RESUMO      ;"
                       "NR-SEQ-FAT     ;"
                       "NOME-ABREV     ;"
                       "NR-PEDCLI      ;"
                       "NR-ENTREGA     ;"
                       "COD-ESTADO     ;"
                       "DT-SEPARACAO   ;"
                       "HR-SEPARACAO   ;"
                       "usuar-separacao;"
                       SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PI-LIBERA-ETIQUETA wgEmbarque 
PROCEDURE PI-LIBERA-ETIQUETA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN 
       BC-ETIQUETA.nr-embarque       = 0  
       BC-ETIQUETA.NR-RESUMO         = 0    
       BC-ETIQUETA.NR-SEQ-FAT        = 0 
       BC-ETIQUETA.NOME-ABREV        = ""
       BC-ETIQUETA.NR-PEDCLI         = ""
       
       BC-ETIQUETA.NR-ENTREGA        = 0
    
       BC-ETIQUETA.COD-ESTADO        = 2 /* etiqueta impressa */

       BC-ETIQUETA.DT-SEPARACAO      = ?
       BC-ETIQUETA.HR-SEPARACAO      = ""
       BC-ETIQUETA.usuar-separacao   = "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piConfereEmbarque wgEmbarque 
PROCEDURE piConfereEmbarque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF  '{&mgcld_version}' >= '2.04'
&THEN

If Session:Set-Wait-State('general') Then.

Status Input c-atualizando.

if not avail tt-pre-fatur 
then 
    return no-apply.


assign vNumAviso = tt-pre-fatur.cdd-embarq
       vnrresumo = tt-pre-fatur.nr-resumo.

Enable btFechaAviso With Frame {&Frame-Name}.

For each tt-confere-aviso:
    Delete tt-confere-aviso.
End.

for each tt-integracao-400:
    delete tt-integracao-400.
end.

for each tt-it-pre-fat:
    delete tt-it-pre-fat.
end.

create tt-integracao-400.
assign 
       tt-integracao-400.cdd-embarq            = vnumaviso
       tt-integracao-400.nr-resumo-ini         = vnrresumo
       tt-integracao-400.nr-resumo-fim         = vnrresumo
       tt-integracao-400.cod-versao-integracao = 1
       tt-integracao-400.ler-it-dep-fat        = no
       l-sit-resumo-1 = no 
       l-sit-resumo-2 = no
       l-sit-resumo-3 = no
       l-sit-resumo-4 = no.

run ftp/ftapi400.p (input  table tt-integracao-400,
                    output table tt-it-pre-fat,
                    output table tt-it-dep-fat,
                    input-output table tt-erro).
                    
ASSIGN de-qt-volume = 0
       de-qt-peso   = 0
       de-qt-metro  = 0.

For each tt-it-pre-fat:

    assign de-total-item = 0.
    
    for each  bc-etiqueta 
         Where bc-etiqueta.nr-embarque = tt-it-pre-fat.cdd-embarq
         and   bc-etiqueta.nr-resumo = tt-it-pre-fat.nr-resumo
         And   bc-etiqueta.it-codigo = tt-it-pre-fat.it-codigo 
         and   bc-etiqueta.referencia = tt-it-pre-fat.cod-refer
         no-lock:
            
         assign de-total-item = de-total-item  + bc-etiqueta.qt-item.  

         FIND ITEM
              WHERE ITEM.it-codigo = bc-etiqueta.it-codigo
              NO-LOCK NO-ERROR.
         
         ASSIGN de-qt-volume = de-qt-volume + 1
                de-qt-peso   = de-qt-peso   + (IF ITEM.un = "KG" THEN bc-etiqueta.qt-item ELSE 0)
                de-qt-metro  = de-qt-metro  + (IF ITEM.un = "M"  THEN bc-etiqueta.qt-item ELSE 0).    
    end.     

    Find First tt-confere-aviso
         where tt-confere-aviso.nr-embarque  =  tt-it-pre-fat.cdd-embarq
         And   tt-confere-aviso.cod-item     =  tt-it-pre-fat.it-codigo 
         and   tt-confere-aviso.cod-refer    =  tt-it-pre-fat.cod-refer 
         no-error.

    Find First Item No-lock         
         Where Item.it-codigo = tt-it-pre-fat.it-codigo.
         
    If  Not Available tt-confere-aviso Then Do:
        Create tt-confere-aviso.
        Assign tt-confere-aviso.nr-embarque   = tt-it-pre-fat.cdd-embarq
               tt-confere-aviso.cod-item      = tt-it-pre-fat.it-codigo
               tt-confere-aviso.cod-refer     = tt-it-pre-fat.cod-refer
               tt-confere-aviso.des-item      = if avail item Then item.desc-item Else ''
               tt-confere-aviso.qtd-faturada  = tt-it-pre-fat.qt-faturada 
               tt-confere-aviso.qtd-embarcada = de-total-item
               tt-confere-aviso.qtd-aberto    = (qtd-faturada - qtd-embarcada)
               tt-confere-aviso.nr-resumo     = tt-it-pre-fat.nr-resumo.
               
    End. /* If  Not Available tt-confere-aviso */
    
    delete tt-it-pre-fat.
    
    If   tt-confere-aviso.qtd-embarcada = 0 Then Do:
         Assign l-sit-resumo-1 = yes.
    End.
    Else 
    If   tt-confere-aviso.qtd-faturada   > tt-confere-aviso.qtd-embarcada And
         tt-confere-aviso.qtd-embarcada <> 0 Then Do:
         Assign l-sit-resumo-2 = yes.
    End.
    Else
    If   tt-confere-aviso.qtd-faturada   = tt-confere-aviso.qtd-embarcada Then Do:
         Assign l-sit-resumo-3 = yes.
    End.
    Else                
    If   tt-confere-aviso.qtd-faturada   < tt-confere-aviso.qtd-embarcada Then Do:
         Assign l-sit-resumo-4 = yes.
    End.
    
End. /* For each tt-aviso-enbarque */

DISP de-qt-volume 
     de-qt-metro
     de-qt-peso 
     WITH FRAME {&FRAME-NAME}.

if  l-sit-resumo-1 = yes 
and l-sit-resumo-2 = no
and l-sit-resumo-3 = no
and l-sit-resumo-4 = no
then 
   assign i-sit-resumo = 12.
else 
if  l-sit-resumo-1 = no
and l-sit-resumo-2 = yes
and l-sit-resumo-3 = no
and l-sit-resumo-4 = no
then 
    assign i-sit-resumo = 6.
else 
if  l-sit-resumo-1 = no
and l-sit-resumo-2 = no
and l-sit-resumo-3 = yes
and l-sit-resumo-4 = no
then 
    assign i-sit-resumo = 2
           tt-pre-fatur.log-2 = yes.
else 
if  l-sit-resumo-1 = no
and l-sit-resumo-2 = no
and l-sit-resumo-3 = no
and l-sit-resumo-4 = yes
then 
    assign i-sit-resumo = 13.
else
if  l-sit-resumo-1 = no
and l-sit-resumo-2 = no
and l-sit-resumo-3 = yes
and l-sit-resumo-4 = yes
then 
    assign i-sit-resumo = 13.
else     
    assign i-sit-resumo = 6.

Find First bc-etiqueta no-lock
     Where bc-etiqueta.nr-embarque   = vNumAviso 
     and   bc-etiqueta.nr-resumo   = vnrresumo
     
     No-Error.
     
If  Available bc-etiqueta Then Do:
        
        if  i-sit-resumo = 2 
        then
            assign  vDesStatusEmbarque:bgcolor In Frame {&Frame-Name}      = 10
                    vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = "Resumo Atendido Total...".
      
        if  i-sit-resumo = 12 
        then
            assign  vDesStatusEmbarque:bgcolor In Frame {&Frame-Name}      = i-sit-resumo
                    vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = "Resumo Aberto...". 

        if  i-sit-resumo = 13
        then
            assign  vDesStatusEmbarque:bgcolor In Frame {&Frame-Name}      = i-sit-resumo
                    vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = "Resumo com Divergàncias...".

      
        if  i-sit-resumo = 6 
        then
            assign  vDesStatusEmbarque:bgcolor In Frame {&Frame-Name}      = 14
                    vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = "Resumo Atendido Parcial...".

     If  bc-etiqueta.cod-estado < 4  Then Do:

        btFechaAviso:ToolTip In Frame {&Frame-Name} = c-fechar.

        assign  c-sit-embarque:bgcolor In Frame {&Frame-Name}      = 12
                c-sit-embarque:Screen-Value In Frame {&Frame-Name} = "N«O LIBERADO FATURAMENTO".

        If  btFechaAviso:Load-Image('image/im-smfec.bmp') Then.
         
     End. /* bc-etiqueta.logico-1 = No */
     ELSE DO:


         If  btFechaAviso:Load-Image('image/im-smabe.bmp') Then.
         /*Assign vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = c-fechado.*/
         Assign btFechaAviso:ToolTip In Frame {&Frame-Name} = c-abrir
                .

         assign  c-sit-embarque:bgcolor In Frame {&Frame-Name}      = 10
                 c-sit-embarque:Screen-Value In Frame {&Frame-Name} = "LIBERADO FATURAMENTO".
         
     END.


End. /* Available bc-etiqueta */      
Else
Do: /* Embarque Aberto */

    If  btFechaAviso:Load-Image('image/im-smfec.bmp') Then.

    assign  vDesStatusEmbarque:bgcolor In Frame {&Frame-Name}      = i-sit-resumo
            vDesStatusEmbarque:Screen-Value In Frame {&Frame-Name} = "Resumo Aberto...". 

    assign  c-sit-embarque:bgcolor In Frame {&Frame-Name}      = 12
            c-sit-embarque:Screen-Value In Frame {&Frame-Name} = "N«O LIBERADO FATURAMENTO".

    
End. /* bc-etiqueta.logico-1 = No */

if  l-mostra-item then do:
    CLOSE QUERY bConfere.
    OPEN QUERY bConfere FOR EACH tt-confere-aviso ,
                        FIRST saldo-estoq NO-LOCK
                        WHERE saldo-estoq.it-codigo = tt-confere-aviso.cod-item
                          AND saldo-estoq.cod-refer = tt-confere-aviso.cod-refer
                        /*  AND saldo-estoq.cod-depos = "ARM" */
                     By tt-confere-aviso.qtd-aberto DESCENDING.    
    /*Status Input c-ultima-atualizacao + " " + String(Today,'99/99/9999') + ' ' + String(Time,'HH:MM:SS').*/
    
end.

If Session:Set-Wait-State('') Then.

&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records wgEmbarque  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-pre-fatur"}
  {src/adm/template/snd-list.i "tt-confere-aviso"}
  {src/adm/template/snd-list.i "saldo-estoq"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed wgEmbarque 
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

