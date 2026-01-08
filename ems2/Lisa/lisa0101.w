&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i lisa0101 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i lisa0101 esp}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{esp/util.i}
{esapi/analisarJsonObject2.i}
{esapi/getNfsRetornoLisaPendLanctoErp.i}
{esapi/integrarRetornoERP.i}
{esbo/boLisa02Cons.i}
{lisa/lisa0101a.i}
DEFINE TEMP-TABLE ttItemNfAux NO-UNDO LIKE ttItemNf.

DEFINE TEMP-TABLE ttSaldoRem
    FIELD pedidoLisaId  AS INT
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD sequencia     AS INT
    FIELD notaRem       AS CHAR
    FIELD qtSaldo       AS DECIMAL
    FIELD qtRegistros   AS INT
    INDEX primario IS PRIMARY pedidoLisaid itCodigo codRefer sequencia notaRem
    .



DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttAux NO-UNDO LIKE ttNfPend .

DEFINE VARIABLE lErro           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iPedidoCorrente AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRetornoLisa    AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtSaldo         AS DECIMAL     NO-UNDO.
/*DEFINE VARIABLE cVlParam        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codEmitPadrao   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cSeriePadrao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperPadrao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
*/
DEFINE VARIABLE hBoDadosRetorno AS HANDLE      NO-UNDO.
DEFINE VARIABLE cErros          AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttQtItemRefAux LIKE ttQtItemRef .
DEFINE TEMP-TABLE ttResultRom    LIKE romaneios_retorno_lisa .
DEFINE TEMP-TABLE ttResultRomAux LIKE romaneios_retorno_lisa .

DEFINE VARIABLE cItem       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRef        AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iCor        AS INTEGER     NO-UNDO.

DEFINE VARIABLE cListaIds   AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brEtqRom

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttResultRomAux ttItemNf ttNfPend ~
ttQtItemRefAux

/* Definitions for BROWSE brEtqRom                                      */
&Scoped-define FIELDS-IN-QUERY-brEtqRom ttResultRomAux.it_codigo ttResultRomAux.cod_refer ttResultRomAux.num_rolo ttResultRomAux.id_etq_lisa ttResultRomAux.quantidade ttResultRomAux.nr_seq_lisa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brEtqRom   
&Scoped-define SELF-NAME brEtqRom
&Scoped-define QUERY-STRING-brEtqRom FOR EACH ttResultRomAux         WHERE ttResultRomAux.retorno_Lisa_Id = iRetornoLisa         AND   ttResultRomAux.it_codigo       = cItem         AND   ttResultRomAux.cod_refer       = cRef
&Scoped-define OPEN-QUERY-brEtqRom OPEN QUERY {&SELF-NAME}     FOR EACH ttResultRomAux         WHERE ttResultRomAux.retorno_Lisa_Id = iRetornoLisa         AND   ttResultRomAux.it_codigo       = cItem         AND   ttResultRomAux.cod_refer       = cRef    .
&Scoped-define TABLES-IN-QUERY-brEtqRom ttResultRomAux
&Scoped-define FIRST-TABLE-IN-QUERY-brEtqRom ttResultRomAux


/* Definitions for BROWSE brItensRetorno                                */
&Scoped-define FIELDS-IN-QUERY-brItensRetorno ttItemNF.sequencia ttItemNf.itCodigo ttItemNf.codRefer ttItemNf.nfOriginal ttItemNf.qtfaturada ttItemNf.qtSaldoDisp ttItemNF.qtExcedente //ttItemNf.qtRetorno ttItemNf.nfSubstituta ttitemNf.codReferSub //ttItemNf.qtNfOriginal //ttItemNf.qtNfSubst   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brItensRetorno   
&Scoped-define SELF-NAME brItensRetorno
&Scoped-define QUERY-STRING-brItensRetorno FOR EACH ttItemNf     WHERE     ( ttItemNf.retornoLisaId = iRetornoLisa     AND   ttItemNf.itCodigo      = cItem     AND   ttItemNf.codRefer      = cRef )     OR (lookup( string(ttItemNF.id), ~
      cListaIds ) > 0         AND ttItemNf.itCodigo = cItem         AND ttItemNf.codReferOri = cRef  )
&Scoped-define OPEN-QUERY-brItensRetorno OPEN QUERY {&SELF-NAME} FOR EACH ttItemNf     WHERE     ( ttItemNf.retornoLisaId = iRetornoLisa     AND   ttItemNf.itCodigo      = cItem     AND   ttItemNf.codRefer      = cRef )     OR (lookup( string(ttItemNF.id), ~
      cListaIds ) > 0         AND ttItemNf.itCodigo = cItem         AND ttItemNf.codReferOri = cRef  ) .
&Scoped-define TABLES-IN-QUERY-brItensRetorno ttItemNf
&Scoped-define FIRST-TABLE-IN-QUERY-brItensRetorno ttItemNf


/* Definitions for BROWSE brRetornos                                    */
&Scoped-define FIELDS-IN-QUERY-brRetornos ttNfPend.dtEmisNota ttNfPend.serie ttNfPend.nrNotaFis ttNfPend.nrPedido ttNfPend.logRe1001 ttNfPend.dtTransacao ttNfPend.prePedido ttNfPend.descrErro   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brRetornos   
&Scoped-define SELF-NAME brRetornos
&Scoped-define QUERY-STRING-brRetornos FOR EACH ttNfPend     WHERE (logSemRe1001:SCREEN-VALUE = 'yes' AND ttNfPend.LOGre1001 = NO)     OR logSemRe1001:SCREEN-VALUE = 'no'     BY ttNfPend.DtEmisNota
&Scoped-define OPEN-QUERY-brRetornos OPEN QUERY {&SELF-NAME} FOR EACH ttNfPend     WHERE (logSemRe1001:SCREEN-VALUE = 'yes' AND ttNfPend.LOGre1001 = NO)     OR logSemRe1001:SCREEN-VALUE = 'no'     BY ttNfPend.DtEmisNota.
&Scoped-define TABLES-IN-QUERY-brRetornos ttNfPend
&Scoped-define FIRST-TABLE-IN-QUERY-brRetornos ttNfPend


/* Definitions for BROWSE brRomaneio                                    */
&Scoped-define FIELDS-IN-QUERY-brRomaneio ttQtItemRefAux.itCodigo ttQtItemRefAux.codRefer ttQtItemRefAux.qt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brRomaneio   
&Scoped-define SELF-NAME brRomaneio
&Scoped-define QUERY-STRING-brRomaneio FOR EACH ttQtItemRefAux WHERE ttQtItemRefAux.retornoLisaId =  iRetornoLisa
&Scoped-define OPEN-QUERY-brRomaneio OPEN QUERY {&SELF-NAME}     FOR EACH ttQtItemRefAux WHERE ttQtItemRefAux.retornoLisaId =  iRetornoLisa .
&Scoped-define TABLES-IN-QUERY-brRomaneio ttQtItemRefAux
&Scoped-define FIRST-TABLE-IN-QUERY-brRomaneio ttQtItemRefAux


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brEtqRom}~
    ~{&OPEN-QUERY-brItensRetorno}~
    ~{&OPEN-QUERY-brRetornos}~
    ~{&OPEN-QUERY-brRomaneio}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-103 RECT-105 btBuscar fiNF ~
fiPedido fiDtIni fiDtFim brRetornos logSemRe1001 tgAtuRe1001 fiDtTransacao ~
brRomaneio brEtqRom brItensRetorno btGetDadosLisa tgSubstAtu 
&Scoped-Define DISPLAYED-OBJECTS fiNF fiPedido fiDtIni fiDtFim logSemRe1001 ~
tgAtuRe1001 fiDtTransacao tgSubstAtu 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAltDtTrans 
     LABEL "Alterar" 
     SIZE 12 BY 1.13.

DEFINE BUTTON btAtuNfSub 
     LABEL "Alterar" 
     SIZE 16 BY 1.13.

DEFINE BUTTON btBuscar 
     LABEL "Buscar" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btConsDadosLisa 
     LABEL "Consultar Dados LISA" 
     SIZE 17.43 BY 1.13.

DEFINE BUTTON btGetDadosLisa 
     LABEL "Buscar Dados LISA" 
     SIZE 17.43 BY 1.13.

DEFINE BUTTON btIntegrarNfsPend 
     LABEL "Integrar Todas NFs Pendentes" 
     SIZE 22.86 BY 1.13.

DEFINE BUTTON btIntegrarSel 
     LABEL "Integrar Nota" 
     SIZE 12.86 BY 1.13.

DEFINE VARIABLE fiDtFim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/2999 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtIni AS DATE FORMAT "99/99/9999":U INITIAL 01/01/23 
     LABEL "Dt.Emis.Retorno" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 NO-UNDO.

DEFINE VARIABLE fiDtTransacao AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Transa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 12.14 BY .88 TOOLTIP "Esta data ser  a data de transa‡Æo no RE1001 na integra‡Æo" NO-UNDO.

DEFINE VARIABLE fiNF AS CHARACTER FORMAT "X(20)":U 
     LABEL "NF Retorno" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fiPedido AS CHARACTER FORMAT "X(20)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-103
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124.86 BY 3.46.

DEFINE RECTANGLE RECT-105
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 126 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE logSemRe1001 AS LOGICAL INITIAL no 
     LABEL "Apenas Sem Re1001 Gerado" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.14 BY .83
     FONT 0 NO-UNDO.

DEFINE VARIABLE tgAtuRe1001 AS LOGICAL INITIAL yes 
     LABEL "Atualizar Re1001" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE tgSubstAtu AS LOGICAL INITIAL yes 
     LABEL "Substituir Atual" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brEtqRom FOR 
      ttResultRomAux SCROLLING.

DEFINE QUERY brItensRetorno FOR 
      ttItemNf SCROLLING.

DEFINE QUERY brRetornos FOR 
      ttNfPend SCROLLING.

DEFINE QUERY brRomaneio FOR 
      ttQtItemRefAux SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brEtqRom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brEtqRom w-livre _FREEFORM
  QUERY brEtqRom DISPLAY
      ttResultRomAux.it_codigo
 ttResultRomAux.cod_refer
 ttResultRomAux.num_rolo
 ttResultRomAux.id_etq_lisa
 ttResultRomAux.quantidade
 ttResultRomAux.nr_seq_lisa
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61.57 BY 4.38
         FONT 1
         TITLE "Etiquetas Romaneio".

DEFINE BROWSE brItensRetorno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brItensRetorno w-livre _FREEFORM
  QUERY brItensRetorno NO-LOCK DISPLAY
      ttItemNF.sequencia COLUMN-LABEL "Seq."
ttItemNf.itCodigo
ttItemNf.codRefer
ttItemNf.nfOriginal
ttItemNf.qtfaturada
ttItemNf.qtSaldoDisp
ttItemNF.qtExcedente
//ttItemNf.qtRetorno
ttItemNf.nfSubstituta
ttitemNf.codReferSub  COLUMN-LABEL "Refer.Sub."

//ttItemNf.qtNfOriginal 
//ttItemNf.qtNfSubst
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 123.43 BY 4.96
         FONT 1
         TITLE "Nota Remessa x Nota Retorno" ROW-HEIGHT-CHARS .5.

DEFINE BROWSE brRetornos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brRetornos w-livre _FREEFORM
  QUERY brRetornos DISPLAY
      ttNfPend.dtEmisNota
ttNfPend.serie
ttNfPend.nrNotaFis
ttNfPend.nrPedido
ttNfPend.logRe1001
ttNfPend.dtTransacao
ttNfPend.prePedido
ttNfPend.descrErro
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 124.72 BY 7.79
         FONT 1
         TITLE "Retornos Pendentes ERP" ROW-HEIGHT-CHARS .46.

DEFINE BROWSE brRomaneio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brRomaneio w-livre _FREEFORM
  QUERY brRomaneio DISPLAY
      ttQtItemRefAux.itCodigo 
ttQtItemRefAux.codRefer
ttQtItemRefAux.qt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61.43 BY 4.5
         FONT 1
         TITLE "Romaneio Retorno".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btBuscar AT ROW 3.92 COL 94.72 WIDGET-ID 4
     fiNF AT ROW 4 COL 15.14 COLON-ALIGNED WIDGET-ID 6
     fiPedido AT ROW 4 COL 35.14 COLON-ALIGNED WIDGET-ID 8
     fiDtIni AT ROW 4 COL 62.43 COLON-ALIGNED WIDGET-ID 10
     fiDtFim AT ROW 4 COL 78.86 COLON-ALIGNED WIDGET-ID 12
     brRetornos AT ROW 6.21 COL 2.29 WIDGET-ID 200
     btIntegrarSel AT ROW 14.08 COL 34.14 WIDGET-ID 14
     btConsDadosLisa AT ROW 14.08 COL 73.43 WIDGET-ID 32
     btAltDtTrans AT ROW 14.13 COL 114.86 WIDGET-ID 30
     logSemRe1001 AT ROW 14.17 COL 2.86 WIDGET-ID 24
     tgAtuRe1001 AT ROW 14.25 COL 50 WIDGET-ID 40
     fiDtTransacao AT ROW 14.29 COL 99.86 COLON-ALIGNED WIDGET-ID 28
     brRomaneio AT ROW 15.33 COL 2.57 WIDGET-ID 400
     brEtqRom AT ROW 15.38 COL 65.14 WIDGET-ID 500
     brItensRetorno AT ROW 20.08 COL 3 WIDGET-ID 300
     btAtuNfSub AT ROW 25.25 COL 3 WIDGET-ID 20
     btIntegrarNfsPend AT ROW 25.42 COL 56.29 WIDGET-ID 16
     btGetDadosLisa AT ROW 25.5 COL 86.29 WIDGET-ID 34
     tgSubstAtu AT ROW 25.67 COL 105.14 WIDGET-ID 38
     rt-button AT ROW 1 COL 1
     RECT-103 AT ROW 2.54 COL 2.14 WIDGET-ID 2
     RECT-105 AT ROW 25.25 COL 82.57 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 126.86 BY 26.13
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Retornos Lisa Pend. ERP"
         HEIGHT             = 26.13
         WIDTH              = 126.86
         MAX-HEIGHT         = 40.5
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 40.5
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB brRetornos fiDtFim f-cad */
/* BROWSE-TAB brRomaneio fiDtTransacao f-cad */
/* BROWSE-TAB brEtqRom brRomaneio f-cad */
/* BROWSE-TAB brItensRetorno brEtqRom f-cad */
/* SETTINGS FOR BUTTON btAltDtTrans IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btAtuNfSub IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btConsDadosLisa IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btIntegrarNfsPend IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       btIntegrarNfsPend:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR BUTTON btIntegrarSel IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brEtqRom
/* Query rebuild information for BROWSE brEtqRom
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH ttResultRomAux
        WHERE ttResultRomAux.retorno_Lisa_Id = iRetornoLisa
        AND   ttResultRomAux.it_codigo       = cItem
        AND   ttResultRomAux.cod_refer       = cRef
   .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brEtqRom */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brItensRetorno
/* Query rebuild information for BROWSE brItensRetorno
     _START_FREEFORM

OPEN QUERY {&SELF-NAME} FOR EACH ttItemNf
    WHERE
    ( ttItemNf.retornoLisaId = iRetornoLisa
    AND   ttItemNf.itCodigo      = cItem
    AND   ttItemNf.codRefer      = cRef )
    OR (lookup( string(ttItemNF.id),cListaIds ) > 0
        AND ttItemNf.itCodigo = cItem
        AND ttItemNf.codReferOri = cRef  ) .
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.itens_pedido_lisa.pedido_lisa_id = 0"
     _Query            is OPENED
*/  /* BROWSE brItensRetorno */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brRetornos
/* Query rebuild information for BROWSE brRetornos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttNfPend
    WHERE (logSemRe1001:SCREEN-VALUE = 'yes' AND ttNfPend.LOGre1001 = NO)
    OR logSemRe1001:SCREEN-VALUE = 'no'
    BY ttNfPend.DtEmisNota.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brRetornos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brRomaneio
/* Query rebuild information for BROWSE brRomaneio
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
    FOR EACH ttQtItemRefAux WHERE ttQtItemRefAux.retornoLisaId =  iRetornoLisa .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brRomaneio */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Retornos Lisa Pend. ERP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Retornos Lisa Pend. ERP */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brItensRetorno
&Scoped-define SELF-NAME brItensRetorno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brItensRetorno w-livre
ON ROW-DISPLAY OF brItensRetorno IN FRAME f-cad /* Nota Remessa x Nota Retorno */
DO:
  IF ttItemNf.qtExcedente > 0 THEN DO:
     ASSIGN iCor = 12.
  END.
  ELSE DO:
     ASSIGN iCor = 0.

  END.
  ASSIGN ttItemNF.sequencia:FGCOLOR    IN BROWSE {&browse-name}   = iCor
         ttItemNf.itCodigo:FGCOLOR     IN BROWSE {&browse-name}   = iCor
         ttItemNf.codRefer:FGCOLOR     IN BROWSE {&browse-name}   = iCor
         ttItemNf.nfOriginal:FGCOLOR   IN BROWSE {&browse-name}   = iCor
         ttItemNf.qtfaturada:FGCOLOR   IN BROWSE {&browse-name}   = iCor
         ttItemNf.qtSaldoDisp:FGCOLOR  IN BROWSE {&browse-name}   = iCor
         ttItemNF.qtExcedente:FGCOLOR  IN BROWSE {&browse-name}   = iCor
         ttItemNf.nfSubstituta:FGCOLOR IN BROWSE {&browse-name}   = iCor
         ttitemNf.codReferSub:FGCOLOR  IN BROWSE {&browse-name}   = iCor .






END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brItensRetorno w-livre
ON VALUE-CHANGED OF brItensRetorno IN FRAME f-cad /* Nota Remessa x Nota Retorno */
DO:
  IF AVAIL ttItemNf THEN DO:
     ASSIGN btAtuNfSub:SENSITIVE = YES.
     IF ttITemNf.qtExcedente = 0 THEN
        ASSIGN btAtuNfSub:SENSITIVE = NO.
  END.
  ELSE DO:
      ASSIGN btAtuNfSub:SENSITIVE = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brRetornos
&Scoped-define SELF-NAME brRetornos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brRetornos w-livre
ON VALUE-CHANGED OF brRetornos IN FRAME f-cad /* Retornos Pendentes ERP */
DO:
  ASSIGN iPedidoCorrente = 0 
         iRetornoLisa    = 0.
  IF AVAIL ttNfPend THEN DO:
     ASSIGN iRetornoLisa = ttNfPend.retornoLisaId .
     FIND pedidos_lisa NO-LOCK
         WHERE pedidos_lisa.nr_pedido = int(ttNfPend.nrPedido) NO-ERROR.
     IF AVAIL pedidos_lisa THEN DO:
        ASSIGN iPedidoCorrente = pedidos_lisa.pedido_Lisa_Id.
        
     END.
     ASSIGN  fiDtTransacao:SCREEN-VALUE = STRING(ttNfPend.dtTransacao).

  END.
  {&open-query-brRomaneio}
  APPLY 'VALUE-CHANGED' TO  BROWSE brRomaneio.
  
  

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brRomaneio
&Scoped-define SELF-NAME brRomaneio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brRomaneio w-livre
ON VALUE-CHANGED OF brRomaneio IN FRAME f-cad /* Romaneio Retorno */
DO:
   
    IF AVAIL ttQtItemRefAux THEN DO:
       ASSIGN cItem = ttQtItemRefAux.itCodigo
              cRef  = ttQtItemRefAux.codRefer .
    END.
    ELSE DO:
        ASSIGN cItem = ''
               cRef  = ''.
    END.

    /*MESSAGE 
        'pedido:'  iPedidoCorrente 
        'retorno:' iRetornoLisa    
        'produto:' cItem           
        'refer.:' cRef
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     
    RUN atuListaIds(iPedidoCorrente).
    {&OPEN-query-brItensRetorno}
    {&OPEN-query-brEtqRom}
    APPLY 'value-changed' TO BROWSE brItensRetorno .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAltDtTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAltDtTrans w-livre
ON CHOOSE OF btAltDtTrans IN FRAME f-cad /* Alterar */
DO:
  IF AVAIL ttNfPend THEN DO:
     ASSIGN ttNfPend.dtTransacao = date(fiDtTransacao:SCREEN-VALUE)
            .
     {&OPEN-query-brRetornos}
     APPLY 'value-changed' TO BROWSE brRetornos.
     

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAtuNfSub
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtuNfSub w-livre
ON CHOOSE OF btAtuNfSub IN FRAME f-cad /* Alterar */
DO:
  
  FIND CURRENT ttItemNF NO-ERROR.
  IF AVAIL ttItemNF  THEN DO:
     RUN exportarTTItemNf('c:\temp\antes.txt').
     RUN lisa/lisa0101a.w(
         ttItemNf.nfRetorno,   
         ttItemNf.itCodigo,
         ttItemNf.codRefer,
         IF ttItemNf.qtNfSubst <> 0 THEN  ttItemNf.qtNfSubst ELSE ttItemNf.qtFaturada,
         INPUT-OUTPUT TABLE ttItemNF,
         ttItemNF.id,
         INPUT-OUTPUT TABLE ttFiltro,
         iRetornoLisa
         ).
     RUN exportarTTItemNf('c:\temp\depois.txt').
     RUN atuListaIds(iPedidoCorrente). 
       
  END.
  
  {&OPEN-query-brItensRetorno}
  APPLY 'value-changed' TO BROWSE brItensRetorno .
  IF CAN-FIND(FIRST ttFiltro) THEN
     ASSIGN btAtuNFSub:SENSITIVE = TRUE.
  


  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBuscar w-livre
ON CHOOSE OF btBuscar IN FRAME f-cad /* Buscar */
DO:                              
  
  RUN utp/ut-acomp.p PERSIST SET h-acomp.
  RUN pi-inicializar IN h-acomp('Buscado NF Retorno').
  RUN pi-acompanhar  IN h-acomp('Lancto Pendentes...').
  RUN esapi/getNfsRetornoLisaPendLanctoErp.p(
                                       fiNf:SCREEN-VALUE,
                                       fiPedido:SCREEN-VALUE,
                                       DATE(fiDtini:SCREEN-VALUE),
                                       DATE(fiDtFim:SCREEN-VALUE),
                                       OUTPUT TABLE ttNfPend  
                                       ).
  
  {&OPEN-query-brRetornos}
  RUN pi-acompanhar  IN h-acomp('Lancto Pendentes...').
  RUN getDadosItensRetorno.
  APPLY 'value-changed' TO BROWSE brRetornos.
  RUN pi-finalizar IN h-acomp.
  ASSIGN btIntegrarSel:SENSITIVE IN FRAME {&FRAME-NAME} = BROWSE brRetornos:QUERY:NUM-RESULTS > 0 
         btAltDtTrans:SENSITIVE  IN FRAME {&FRAME-NAME} = BROWSE brRetornos:QUERY:NUM-RESULTS > 0 
         btConsDadosLisa:SENSITIVE  IN FRAME {&FRAME-NAME} = BROWSE brRetornos:QUERY:NUM-RESULTS > 0
         btAtuNfSub:SENSITIVE  IN FRAME {&FRAME-NAME} = BROWSE brItensRetorno:QUERY:NUM-RESULTS > 0.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConsDadosLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConsDadosLisa w-livre
ON CHOOSE OF btConsDadosLisa IN FRAME f-cad /* Consultar Dados LISA */
DO:
  RUN utp/ut-acomp.p PERSIST SET h-acomp.
  RUN pi-inicializar IN h-acomp("BUSCANDO INFORMA€åES NA LISA").
  IF ttNFPend.prePedido = 0 THEN DO:
   MESSAGE "Pedido de Venda sem Pr‚-Pedido"
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN NO-APPLY.

  END.

  RUN pi-acompanhar  IN h-acomp("Pr‚-Pedido:" + string(ttNFPend.prePedido,'999999')).
  RUN lisa/consultarPedVenda.p(string(ttNFPend.prePedido,'999999'),
                    OUTPUT cErros,
                    OUTPUT TABLE ttJson
                    ).
  IF cErros <> '' THEN DO:
     MESSAGE cErros
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     RUN pi-finalizar IN h-acomp.
  END.
    

  ELSE DO:
    /*ASSIGN cArquivo = SESSION:TEMP-DIRECTORY + "cons_pedido_lisa_" + STRING(TIME) + ".csv".
    OUTPUT TO value(cArquivo).
    FOR EACH  ttjson.
        EXPORT DELIMITER ";" ttjson.
    END.
    OUTPUT CLOSE.

    OS-COMMAND SILENT VALUE("start excel " + cArquivo).
    */
    RUN pi-finalizar IN h-acomp.
    RUN lisa/dadosPrePedidoLisa.w(INPUT TABLE ttJson).

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGetDadosLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGetDadosLisa w-livre
ON CHOOSE OF btGetDadosLisa IN FRAME f-cad /* Buscar Dados LISA */
DO:
  RUN utp/ut-acomp.p PERSIST SET h-acomp.
  RUN pi-inicializar IN h-acomp('Buscado NF Retorno').
  
  IF AVAIL ttNfPend THEN DO:
     IF tgSubstAtu:SCREEN-VALUE = 'yes'  THEN DO:
        RUN esapi/excluirDadosPedidoIMA.p(ttNfPend.nrPedido).
        FOR EACH ttItemNf
        WHERE  ttItemNf.retornoLisaId = iRetornoLisa.
           DELETE ttItemNf.
        END.
     END.
     RUN esbo/boDadosRetorno.p PERSIST SET hBoDadosRetorno.
     RUN iniciar        IN hBoDadosRetorno.
     RUN setValsIni     IN hBoDadosRetorno.
     RUN setProp        IN hBoDadosRetorno('nrPedido',1,ttnfPend.nrPedido).
     RUN setProp        IN hBoDadosRetorno('nrPedido',2,ttnfPend.nrPedido).
     RUN setSitsPedLisa IN hBoDadosRetorno('A EXPEDIR,FINALIZADO'). 
     RUN exec          IN hBoDadosRetorno.
     //RUN getTTResult   IN hBoDadosRetorno(OUTPUT TABLE ttResult).
     //RUN finalizar     IN hBoDadosRetorno. verificar o porque de dar erro no finalizar
     //{esp/exportarEAbrirTabelaCsv3.i ttResult " " " " "ttResult"}
     RUN getDadosItensRetornoCorrente(ROWID(ttNfPend)).
     
     IF VALID-HANDLE(hBoDadosRetorno) THEN
       DELETE PROCEDURE hBoDadosRetorno.
     END.
     
     RUN pi-acompanhar IN h-acomp('Recalculando saldo Disp.').
     RUN criarTtSaldoRem.
     RUN calcSaldoTTItemNF.
     RUN pi-finalizar IN h-acomp.
     APPLY 'value-changed' TO BROWSE brRetornos.
     //APPLY 'value-changed' TO BROWSE brItensRetorno.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btIntegrarNfsPend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btIntegrarNfsPend w-livre
ON CHOOSE OF btIntegrarNfsPend IN FRAME f-cad /* Integrar Todas NFs Pendentes */
DO:
  
  IF CAN-FIND( FIRST ttNfPend) THEN DO:
     RUN utp/ut-acomp.p PERSIST SET h-acomp.
     RUN pi-inicializar IN h-acomp('Integrando Retorno Lisa').
     RUN pi-acompanhar IN h-acomp('Todas as Nfs Pendentes').
     RUN esapi/integrarRetornoERP.p(TABLE ttNfPend).
     RUN pi-finalizar IN h-acomp.
     APPLY 'choose' TO btBuscar.
  END.
  IF lErro  THEN
        MESSAGE  'Existiram erros durante a integra‡Æo. Verifique o LOG NO c:\temp'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btIntegrarSel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btIntegrarSel w-livre
ON CHOOSE OF btIntegrarSel IN FRAME f-cad /* Integrar Nota */
DO:
  
  IF AVAIL ttNfPend THEN DO:
     //RUN gravarItensRetornoLisa. TODO
     EMPTY TEMP-TABLE ttAux.
     CREATE ttAux.
     BUFFER-COPY ttnfPend TO ttAux.
     RUN utp/ut-acomp.p PERSIST SET h-acomp.
     RUN pi-inicializar IN h-acomp('Integrando Retorno Lisa').
     RUN pi-acompanhar IN h-acomp('NF:' + ttnfPend.nrNotaFis).
     RUN gerarTtItemNfAux(int(ttNfPend.nrPedido)).
     RUN esapi/integrarRetornoERP.p(TABLE ttAux, TABLE ttItemNfAux, OUTPUT lErro, 
                                    LOGICAL(INPUT tgAtuRe1001:SCREEN-VALUE)).
     RUN pi-finalizar IN h-acomp.
     IF lErro THEN
        MESSAGE 'Existiram erros durante a integra‡Æo. Verifique o LOG NO c:\temp'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     ELSE DO:
         APPLY 'choose' TO btBuscar.
     END.
     
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPedido w-livre
ON RETURN OF fiPedido IN FRAME f-cad /* Pedido */
DO:
  APPLY 'choose' TO btBuscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME logSemRe1001
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL logSemRe1001 w-livre
ON VALUE-CHANGED OF logSemRe1001 IN FRAME f-cad /* Apenas Sem Re1001 Gerado */
DO:
  {&open-query-brRetornos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brEtqRom
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.08 , 110.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             btBuscar:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE atuListaIds w-livre 
PROCEDURE atuListaIds :
DEFINE INPUT  PARAMETER pPedidoLisaId AS INTEGER     NO-UNDO.
ASSIGN cListaIds = ''.
    FOR EACH ttFiltro
        WHERE ttFiltro.agrup_id = pPedidoLisaId:
        RUN incrValor(INPUT-OUTPUT cListaIds,string(ttFiltro.id),",").
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcSaldoTTItemNF w-livre 
PROCEDURE calcSaldoTTItemNF :
DEFINE VARIABLE iQt AS INTEGER     NO-UNDO.
    
    OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY +  'log_calc_saldo_tt_item_nf' + STRING(RANDOM(1,99899)) + STRING(TIME) + ".txt").
    FOR EACH ttSaldoRem:
        PUT UNFORM FILL("=",150) SKIP.
        PUT UNFORM "Produto" ttsaldoRem.itCodigo 
            "Refer."  ttSaldoRem.codRefer
            "seq."    ttSaldoRem.sequencia
            "nota"    ttSaldoRem.notaRem SKIP.
        PUT UNFORM FILL("-",150) SKIP. 
        ASSIGN iQt = 0.
        FOR EACH ttItemNF 
            WHERE ttSaldoRem.pedidoLisaId = ttItemNF.pedidoLisaId
            AND   ttSaldoRem.itCodigo     = ttItemNf.itCodigo
            AND   ttSaldoRem.codRefer     = ttItemNf.codRefer
            AND   ttSaldoRem.sequencia    = ttItemNF.sequencia
            AND   ttSaldoRem.notaRem      = ttItemNf.nfOriginal:
            ASSIGN iQt = iQt + 1.
            
    
            //calcular saldo excedente
            ASSIGN ttItemNF.qtExcedente = ttItemNF.qtFaturada - ttSaldoRem.qtSaldo   .
            PUT UNFORM "Qt.Faturada:"  ttItemNF.qtFaturada 
            "Qt.Saldo:"    ttSaldoRem.qtSaldo
            "Qt.Excedente:" ttItemNF.qtExcedente
            SKIP.
            
            
            
            IF ttItemNF.qtExcedente < 0 THEN DO:
               ASSIGN ttItemNF.qtExcedente = 0.
               PUT UNFORM "qtExcente menor que zero. Setada como zero para evitar quantidade negativa" SKIP.
            END.                                                                                                 
            
            
               
            IF ttItemNF.qtExcedente > ttItemNf.qtFaturada THEN DO:
            
               PUT UNFORM "Qt.Excendente:" ttItemNF.qtExcedente " maior que a qt.faturada:"  ttItemNf.qtFaturada 
                "->Qt.Excedente igualada a qt.faturada" SKIP.            
               ASSIGN ttItemNF.qtExcedente = ttItemNf.qtFaturada.
            END.
               

            //calcula saldo disponivel
            ASSIGN ttItemNF.qtSaldoDisp = ttSaldoRem.qtSaldo.
            PUT UNFORM "Qt.Saldo Disp no item ref atualizado para a quantidade de saldo da ttsaldorem:" ttSaldoRem.qtSaldo SKIP.
            
            PUT UNFORM "Contador:" iQT "qt.registros:" ttSaldoRem.qtRegistros  SKIP.
            //limitar saldo quando nÆo for o ultimo item
            IF iQt < ttSaldoRem.qtRegistros THEN DO:
               PUT UNFORM " NÆo ‚ o ultimo item " SKIP.
               IF ttItemNf.qtSaldoDisp > ttItemNf.qtFaturada THEN DO:
                  ASSIGN ttItemNf.qtSaldoDisp = ttItemNf.qtFaturada.
                  PUT UNFORM "Qt.Saldo Disp.:" ttItemNf.qtSaldoDisp " maior que a quantidade faturada:" ttItemNf.qtFaturada SKIP.                  
               END.               
               
               PUT UNFORM "Qt.Saldo:" ttSaldoRem.qtSaldo " ajustado para:" ttSaldoRem.qtSaldo - ttItemNf.qtFaturada SKIP.               
               //ajusta saldo 
               ASSIGN ttSaldoRem.qtSaldo = ttSaldoRem.qtSaldo - ttItemNf.qtFaturada.
               
            END.
            IF ttItemNf.qtSaldoDisp < 0 THEN DO:
               ASSIGN ttItemNF.qtSaldoDisp = 0.
               PUT UNFORM "qt saldo disp. do item ref menor que zero e ajustada para zero" SKIP.
            END.
               

        END.
    END.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criarTtSaldoRem w-livre 
PROCEDURE criarTtSaldoRem :
EMPTY TEMP-TABLE ttSaldoRem.
FOR EACH ttItemNF:
        FIND ttSaldoRem
            WHERE ttSaldoRem.pedidoLisaId   = ttItemNf.pedidoLisaId
            AND   ttSaldoRem.itCodigo       = ttItemNf.itCodigo
            AND   ttSaldoRem.codRefer       = ttItemNf.codRefer
            AND   ttSaldoRem.sequencia      = ttItemNf.sequencia
            AND   ttSaldoRem.notaRem        = ttItemNf.nfOriginal    
            NO-ERROR.
        IF NOT AVAIL ttSaldoRem THEN DO:
           CREATE ttSaldoRem.
           ASSIGN ttSaldoRem.pedidoLisaId   = ttItemNf.pedidoLisaId 
                  ttSaldoRem.itCodigo       = ttItemNf.itCodigo     
                  ttSaldoRem.codRefer       = ttItemNf.codRefer     
                  ttSaldoRem.sequencia      = ttItemNf.sequencia    
                  ttSaldoRem.notaRem        = ttItemNf.nfOriginal   .
        END.
        ASSIGN ttSaldoRem.qtRegistros = ttSaldoRem.qtRegistros + 1.
    END.

    FOR EACH ttSaldoRem:

        RUN esapi/getSaldoTercDoctoLisa.p(
            string(int(ttSaldoRem.notaRem),'9999999'),   
            ttSaldoRem.itCodigo, 
            ttSaldoRem.sequencia,        
            OUTPUT ttSaldoRem.qtSaldo ).

    END.

    OUTPUT TO c:\temp\ttSaldoRem.txt.

        FOR EACH ttSaldoRem:
            DISP ttSaldorem .
        END.

    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY fiNF fiPedido fiDtIni fiDtFim logSemRe1001 tgAtuRe1001 fiDtTransacao 
          tgSubstAtu 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-103 RECT-105 btBuscar fiNF fiPedido fiDtIni fiDtFim 
         brRetornos logSemRe1001 tgAtuRe1001 fiDtTransacao brRomaneio brEtqRom 
         brItensRetorno btGetDadosLisa tgSubstAtu 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportarTTitemNF w-livre 
PROCEDURE exportarTTitemNF :
DEFINE INPUT  PARAMETER pNomeArquivo AS CHARACTER   NO-UNDO.
DEFINE BUFFER bf FOR ttItemNF.
OUTPUT TO VALUE(pNomeArquivo).
  FOR EACH bf
   // WHERE lookup(string(ttItemNF.id),cListaIds  ) > 0
   .
   EXPORT DELIMITER "|" bf.
  END.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gerarTtItemNfAux w-livre 
PROCEDURE gerarTtItemNfAux :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
EMPTY TEMP-TABLE ttitemNfAux.
FIND pedidos_lisa NO-LOCK
    WHERE pedidos_lisa.nr_pedido = pNrPedido
    NO-ERROR.
IF AVAIL pedidos_lisa THEN DO:
   FOR EACH ttitemNf
       WHERE ttItemNf.pedidoLisaId = pedidos_lisa.pedido_lisa_id.
       CREATE ttItemNfAux.
       BUFFER-COPY ttItemNf TO ttitemNfAux.
       
   END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDadosItensComum w-livre 
PROCEDURE getDadosItensComum :
DEFINE BUFFER bf FOR ttITemNF.
RUN pi-acompanhar  IN h-acomp('Buscando Itens do Pedido:' + ttNfPend.nrPedido).
    FIND pedidos_lisa NO-LOCK
        WHERE pedidos_lisa.nr_pedido = int(ttnfPend.nrPedido)
        NO-ERROR.
    IF AVAIL pedidos_lisa THEN DO:
       FOR EACH ttItemNf
           WHERE ttItemNf.pedidoLisaId = pedidos_lisa.pedido_lisa_id.
           DELETE ttItemNF .
       END.
       FOR EACH itens_pedido_lisa NO-LOCK
           WHERE itens_pedido_lisa.pedido_lisa_id = pedidos_lisa.pedido_lisa_id.
           FIND ttItemNF
               WHERE  ttItemNf.pedidoLisaId     = pedidos_lisa.pedido_lisa_id
               AND    ttItemNf.itemPedidoLisaId = itens_pedido_lisa.ITEM_pedido_lisa_id
               AND    ttItemNf.nfOriginal       = itens_pedido_lisa.nf_origem   
               AND    ttItemNf.itCodigo         = itens_pedido_lisa.it_codigo   
               AND    ttItemNf.codRefer         = itens_pedido_lisa.cod_refer   
               AND    ttItemNf.sequencia        = INT(itens_pedido.item_nf_origem) * 10 
               AND    ttItemNf.retornoLisaId    = ttNfPend.retornoLisaId
               AND    ttItemNf.nfRetorno        = itens_pedido_lisa.nf_retorno
               AND    ttItemNf.codReferRet      = itens_pedido_lisa.cod_refer
               NO-ERROR.
           IF NOT AVAIL ttItemNF THEN DO:
              FIND LAST bf USE-INDEX ind-id NO-ERROR.

              CREATE ttItemNf.
              ASSIGN ttITemNf.id               = IF AVAIL bf THEN bf.id + 1 ELSE 1
                     ttItemNf.pedidoLisaId     = pedidos_lisa.pedido_lisa_id
                     ttItemNf.itemPedidoLisaId = itens_pedido_lisa.ITEM_pedido_lisa_id
                     ttItemNf.nfOriginal       = itens_pedido_lisa.nf_origem
                     ttItemNf.itCodigo         = itens_pedido_lisa.it_codigo
                     ttItemNf.codRefer         = itens_pedido_lisa.cod_refer
                     ttItemNf.sequencia        = INT(itens_pedido.item_nf_origem) * 10 
                     ttItemNf.retornoLisaId    = ttNfPend.retornoLisaId
                     ttItemNf.nfRetorno        = itens_pedido_lisa.nf_retorno
                     ttItemNf.codReferRet      = itens_pedido_lisa.cod_refer 
                     ttItemNf.logCalcSist      = YES
                     .
           END.
           ASSIGN  ttItemNf.qtfaturada       = ttItemNf.qtfaturada + itens_pedido_lisa.qt_faturada .
       END.
    END.
   
   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDadosItensRetorno w-livre 
PROCEDURE getDadosItensRetorno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.   
RUN esbo/boLisa02cons.p PERSIST SET hBo.
RUN iniciar     IN HBo.
RUN setValsIni  IN hBo.
EMPTY TEMP-TABLE ttQtItemRefAux .
EMPTY TEMP-TABLE ttResultRomAux .
FOR EACH ttNfPend:
     RUN getDadosItensComum.
     RUN setProp IN hBo('retornoLisaId',1,ttNfPend.retornoLisaId).
     RUN setProp IN hBo('retornoLisaId',2,ttNfPend.retornoLisaId).
     RUN exec IN hBo.
     RUN gerarTtQtItemRef IN hBo.
     RUN getTtQtItemRef   IN hBo(OUTPUT TABLE ttQtItemRef).
     FOR EACH ttQtItemRef .
        CREATE ttQtItemRefAux.
        BUFFER-COPY ttQtItemRef TO ttQtItemRefAux . 
     END.
     RUN getTTResult      IN hBo(OUTPUT TABLE ttResultRom).
     FOR EACH  ttResultRom:
         CREATE ttResultRomAux.
         BUFFER-COPY ttResultRom TO ttResultRomAux.
     END.
 END.

FOR EACH ttItemNF:
     FIND ttQTItemRef
         WHERE ttQtItemRef.retornoLisaId = ttItemNf.retornolisaId
         AND   ttqtItemRef.itCodigo      = ttItemNf.itCodigo
         AND   ttQtItemRef.codRefer      = ttitemNf.codRefer
         NO-ERROR.
     IF AVAIL ttQtItemRef THEN DO:
        ASSIGN ttItemNf.qtRetorno = ttQtItemRef.qt .
     END.
END.
RUN criarTtSaldoRem.
RUN calcSaldoTTItemNF.
RUN finalizar IN hBo.
{esp/exportarTabelacsv3.i ttItemNF " " " " "  "ttItemNf" }
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDadosItensRetornoCorrente w-livre 
PROCEDURE getDadosItensRetornoCorrente :
DEFINE INPUT  PARAMETER pRowid AS ROWID      NO-UNDO.
FIND ttNfPend
    WHERE rowid(ttNfPend) = pRowid NO-ERROR.

IF AVAIL  ttNfPend THEN DO:
    RUN getDadosItensComum.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /*IF VALID-HANDLE(hBoConsParam) THEN
     DELETE PROCEDURE hBoConsParam.*/
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "lisa0101" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  
  /* Code placed here will execute AFTER standard behavior.    */
  

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttQtItemRefAux"}
  {src/adm/template/snd-list.i "ttNfPend"}
  {src/adm/template/snd-list.i "ttItemNf"}
  {src/adm/template/snd-list.i "ttResultRomAux"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

