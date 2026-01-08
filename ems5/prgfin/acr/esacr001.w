&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esacr001 11.05.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cListaPortadores        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaPortadoresCodigo  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaEspecies          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaEspeciesCodigo    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabCorrente          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCodCliente             AS INTEGER     NO-UNDO.
/*DEFINE VARIABLE l-Filtro                AS LOGICAL     NO-UNDO.*/
{prgfin/acr/esacr001.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_antecipacoes tt_integr_acr_cheq ~
tt_integr_acr_liq_item_lote_3 tt_titulos

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt_antecipacoes.cod_espec_docto tt_antecipacoes.cod_ser_docto tt_antecipacoes.cod_tit_acr tt_antecipacoes.cod_parcela tt_antecipacoes.cod_portador tt_antecipacoes.valor_abatimento   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt_antecipacoes
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt_antecipacoes.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt_antecipacoes
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt_antecipacoes


/* Definitions for BROWSE br_cheques                                    */
&Scoped-define FIELDS-IN-QUERY-br_cheques 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_cheques 
&Scoped-define SELF-NAME br_cheques
&Scoped-define QUERY-STRING-br_cheques FOR EACH tt_integr_acr_cheq
&Scoped-define OPEN-QUERY-br_cheques OPEN QUERY {&SELF-NAME} FOR EACH tt_integr_acr_cheq.
&Scoped-define TABLES-IN-QUERY-br_cheques tt_integr_acr_cheq
&Scoped-define FIRST-TABLE-IN-QUERY-br_cheques tt_integr_acr_cheq


/* Definitions for BROWSE br_depositos                                  */
&Scoped-define FIELDS-IN-QUERY-br_depositos   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_depositos   
&Scoped-define SELF-NAME br_depositos
&Scoped-define QUERY-STRING-br_depositos FOR EACH tt_integr_acr_liq_item_lote_3
&Scoped-define OPEN-QUERY-br_depositos OPEN QUERY {&SELF-NAME} FOR EACH tt_integr_acr_liq_item_lote_3.
&Scoped-define TABLES-IN-QUERY-br_depositos tt_integr_acr_liq_item_lote_3
&Scoped-define FIRST-TABLE-IN-QUERY-br_depositos tt_integr_acr_liq_item_lote_3


/* Definitions for BROWSE br_titulos                                    */
&Scoped-define FIELDS-IN-QUERY-br_titulos tt_titulos.l-considera tt_titulos.cod_espec_docto tt_titulos.cod_ser_docto tt_titulos.cod_tit_acr tt_titulos.cod_parcela tt_titulos.cod_portador tt_titulos.dat_vencto_tit_acr tt_titulos.val_sdo_tit_acr tt_titulos.vl-juros tt_titulos.vl-despesa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_titulos tt_titulos.l-considera tt_titulos.vl-juros tt_titulos.vl-despesa   
&Scoped-define ENABLED-TABLES-IN-QUERY-br_titulos tt_titulos
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_titulos tt_titulos
&Scoped-define SELF-NAME br_titulos
&Scoped-define QUERY-STRING-br_titulos FOR EACH tt_titulos
&Scoped-define OPEN-QUERY-br_titulos OPEN QUERY {&SELF-NAME} FOR EACH tt_titulos.
&Scoped-define TABLES-IN-QUERY-br_titulos tt_titulos
&Scoped-define FIRST-TABLE-IN-QUERY-br_titulos tt_titulos


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br_titulos}

/* Definitions for FRAME fr_antecipacao                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fr_antecipacao ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Definitions for FRAME fr_cheques                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fr_cheques ~
    ~{&OPEN-QUERY-br_cheques}

/* Definitions for FRAME fr_deposito                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fr_deposito ~
    ~{&OPEN-QUERY-br_depositos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 sl_especie sl_portador ~
dt_vencto_ini dt_vencto_fim fi_nom_abrev bt-efetivar dt_emissao_fim ~
dt_emissao_ini bt_filtrar dt_base br_titulos dt_media_titulos ~
dt_media_pagtos rs_tipo_pagamento bt-efetivar-2 fi_tot_titulos ~
fi_tot_cheque fi_tot_deposito fi_tot_antecipacao fi_saldo 
&Scoped-Define DISPLAYED-OBJECTS sl_especie sl_portador dt_vencto_ini ~
dt_vencto_fim fi_nom_abrev desc_cliente dt_emissao_fim dt_emissao_ini ~
dt_base dt_media_titulos dt_media_pagtos rs_tipo_pagamento fi_tot_titulos ~
fi_tot_cheque fi_tot_deposito fi_tot_antecipacao fi_saldo 

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
DEFINE BUTTON bt-efetivar 
     IMAGE-UP FILE "adeicon/check.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Processar Recebto" 
     SIZE 7.43 BY 1.25.

DEFINE BUTTON bt-efetivar-2 
     IMAGE-UP FILE "adeicon/cross.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Processar Recebto" 
     SIZE 7.43 BY 1.25.

DEFINE BUTTON bt_filtrar 
     LABEL "Buscar Titulos" 
     SIZE 13 BY 1.25.

DEFINE VARIABLE desc_cliente AS CHARACTER FORMAT "X(20)":U INITIAL "Selecione um Cliente V†lido" 
     VIEW-AS FILL-IN 
     SIZE 24.72 BY .88 NO-UNDO.

DEFINE VARIABLE dt_base AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Base" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt_emissao_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_emissao_ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Dt.Emiss∆o de" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_media_pagtos AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Dt.MÇdia Pagtos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_media_titulos AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Dt.MÇdia Titulos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_vencto_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE dt_vencto_ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Dt.Vencto de" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_nom_abrev AS CHARACTER FORMAT "X(20)":U 
     LABEL "Nom.Abrev.Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_saldo AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Projetado" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_tot_antecipacao AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Antecipaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_tot_cheque AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_tot_deposito AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Dep¢sito/Dinheiro" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_tot_titulos AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Titulos" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE rs_tipo_pagamento AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Cheque", 1,
"Dep¢sito/Dinheiro", 2,
"Antecipaá∆o", 3
     SIZE 38.57 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 6.17.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 110 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE sl_especie AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 4.75 NO-UNDO.

DEFINE VARIABLE sl_portador AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 23.57 BY 4.75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt_antecipacoes SCROLLING.

DEFINE QUERY br_cheques FOR 
      tt_integr_acr_cheq SCROLLING.

DEFINE QUERY br_depositos FOR 
      tt_integr_acr_liq_item_lote_3 SCROLLING.

DEFINE QUERY br_titulos FOR 
      tt_titulos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt_antecipacoes.cod_espec_docto  
tt_antecipacoes.cod_ser_docto    
tt_antecipacoes.cod_tit_acr      
tt_antecipacoes.cod_parcela      
tt_antecipacoes.cod_portador     
tt_antecipacoes.valor_abatimento
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 7
         TITLE "Antecipaá∆o" FIT-LAST-COLUMN.

DEFINE BROWSE br_cheques
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_cheques w-livre _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 6.75
         TITLE "Cheques" ROW-HEIGHT-CHARS .42 FIT-LAST-COLUMN.

DEFINE BROWSE br_depositos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_depositos w-livre _FREEFORM
  QUERY br_depositos DISPLAY
      
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 7
         TITLE "Dep¢sitos" FIT-LAST-COLUMN.

DEFINE BROWSE br_titulos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_titulos w-livre _FREEFORM
  QUERY br_titulos DISPLAY
      tt_titulos.l-considera COLUMN-LABEL 'Considera'
tt_titulos.cod_espec_docto
tt_titulos.cod_ser_docto
tt_titulos.cod_tit_acr
tt_titulos.cod_parcela
tt_titulos.cod_portador
tt_titulos.dat_vencto_tit_acr
tt_titulos.val_sdo_tit_acr
tt_titulos.vl-juros COLUMN-LABEL 'Juros'
tt_titulos.vl-despesa COLUMN-LABEL 'Despesas'
ENABLE tt_titulos.l-considera
tt_titulos.vl-juros
tt_titulos.vl-despesa
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 6
         FONT 1
         TITLE "Titulos em Aberto a Liquidar" ROW-HEIGHT-CHARS .54 TOOLTIP "Titulos em Aberto a Liquidar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     sl_especie AT ROW 3.75 COL 60 NO-LABEL WIDGET-ID 48
     sl_portador AT ROW 3.75 COL 85 NO-LABEL WIDGET-ID 50
     dt_vencto_ini AT ROW 3.92 COL 15.29 COLON-ALIGNED WIDGET-ID 12
     dt_vencto_fim AT ROW 3.92 COL 40 COLON-ALIGNED WIDGET-ID 14
     fi_nom_abrev AT ROW 5 COL 15 COLON-ALIGNED WIDGET-ID 16
     bt-efetivar AT ROW 1.13 COL 1.43 WIDGET-ID 66
     desc_cliente AT ROW 5 COL 29.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     dt_emissao_fim AT ROW 6.04 COL 40 COLON-ALIGNED WIDGET-ID 10
     dt_emissao_ini AT ROW 6.13 COL 15 COLON-ALIGNED WIDGET-ID 4
     bt_filtrar AT ROW 7.08 COL 43 WIDGET-ID 24
     dt_base AT ROW 7.25 COL 15 COLON-ALIGNED WIDGET-ID 30
     br_titulos AT ROW 9.25 COL 3 WIDGET-ID 200
     dt_media_titulos AT ROW 15.67 COL 65.86 COLON-ALIGNED WIDGET-ID 56
     dt_media_pagtos AT ROW 15.67 COL 93.57 COLON-ALIGNED WIDGET-ID 58
     rs_tipo_pagamento AT ROW 15.75 COL 5.43 NO-LABEL WIDGET-ID 42
     bt-efetivar-2 AT ROW 1.13 COL 8.86 WIDGET-ID 68
     fi_tot_titulos AT ROW 24.67 COL 5.57 COLON-ALIGNED WIDGET-ID 62
     fi_tot_cheque AT ROW 24.71 COL 23.72 COLON-ALIGNED WIDGET-ID 34
     fi_tot_deposito AT ROW 24.71 COL 49 COLON-ALIGNED WIDGET-ID 38
     fi_tot_antecipacao AT ROW 24.71 COL 70.86 COLON-ALIGNED WIDGET-ID 40
     fi_saldo AT ROW 24.71 COL 96.43 COLON-ALIGNED WIDGET-ID 64
     "Portador" VIEW-AS TEXT
          SIZE 15 BY .58 AT ROW 3 COL 85.86 WIDGET-ID 54
     "Especie" VIEW-AS TEXT
          SIZE 15 BY .58 AT ROW 3.04 COL 60.29 WIDGET-ID 52
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.83 COL 3.43 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110 BY 24.75
         FONT 1 WIDGET-ID 100.

DEFINE FRAME fr_deposito
     br_depositos AT ROW 1.25 COL 2 WIDGET-ID 800
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 16.79
         SIZE 109 BY 7.71 WIDGET-ID 500.

DEFINE FRAME fr_cheques
     br_cheques AT ROW 1.29 COL 3 WIDGET-ID 400
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.57 ROW 16.75
         SIZE 108 BY 7.5 WIDGET-ID 300.

DEFINE FRAME fr_antecipacao
     BROWSE-2 AT ROW 1.25 COL 2 WIDGET-ID 900
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 16.75
         SIZE 109 BY 7.75 WIDGET-ID 700.


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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 24.75
         WIDTH              = 110
         MAX-HEIGHT         = 31.33
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 31.33
         VIRTUAL-WIDTH      = 195.14
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
/* REPARENT FRAME */
ASSIGN FRAME fr_antecipacao:FRAME = FRAME f-cad:HANDLE
       FRAME fr_cheques:FRAME = FRAME f-cad:HANDLE
       FRAME fr_deposito:FRAME = FRAME f-cad:HANDLE.

/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br_titulos dt_base f-cad */
/* SETTINGS FOR FILL-IN desc_cliente IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME fr_antecipacao
                                                                        */
/* BROWSE-TAB BROWSE-2 1 fr_antecipacao */
/* SETTINGS FOR FRAME fr_cheques
                                                                        */
/* BROWSE-TAB br_cheques 1 fr_cheques */
/* SETTINGS FOR FRAME fr_deposito
                                                                        */
/* BROWSE-TAB br_depositos 1 fr_deposito */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_antecipacoes.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_cheques
/* Query rebuild information for BROWSE br_cheques
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_integr_acr_cheq.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_cheques */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_depositos
/* Query rebuild information for BROWSE br_depositos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_integr_acr_liq_item_lote_3.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_depositos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_titulos
/* Query rebuild information for BROWSE br_titulos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_titulos.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br_titulos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-efetivar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-efetivar w-livre
ON CHOOSE OF bt-efetivar IN FRAME f-cad /* Processar Recebto */
DO:
  FIND FIRST tt_titulos
      WHERE tt_titulos.l-considera = YES NO-LOCK  NO-ERROR.
  IF NOT AVAIL tt_titulos THEN DO:
     MESSAGE "ê necess†rio pelo menos 1 titulo com o campo 'Considera' com o valor 'sim' para prosseguir a efetivaá∆o do recebimento do titulo !!!  "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.

  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-efetivar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-efetivar-2 w-livre
ON CHOOSE OF bt-efetivar-2 IN FRAME f-cad /* Processar Recebto */
DO:
 EMPTY TEMP-TABLE tt_titulos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_filtrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_filtrar w-livre
ON CHOOSE OF bt_filtrar IN FRAME f-cad /* Buscar Titulos */
DO:
  
  IF fi_nom_abrev:SCREEN-VALUE IN FRAME  {&FRAME-NAME} = '' THEN DO:
     MESSAGE 'Favor informar o cliente...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi_nom_abrev IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY. 
  END.

  RUN prgint/utb/buscarEstabCorrente.p(OUTPUT cEstabCorrente).
  RUN prgfin/acr/buscarCodCliente.p(fi_nom_abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                  OUTPUT  iCodCliente).

  RUN buscarTitulosFiltro
      ( cEstabCorrente,
        iCodCliente,
        sl_especie:SCREEN-VALUE IN FRAME {&FRAME-NAME} ,
        sl_portador:SCREEN-VALUE IN FRAME {&FRAME-NAME},
        INPUT FRAME {&FRAME-NAME} dt_emissao_ini,
        INPUT FRAME {&FRAME-NAME} dt_emissao_fim,
        INPUT FRAME {&FRAME-NAME} dt_vencto_ini,
        INPUT FRAME {&FRAME-NAME} dt_vencto_fim
      ).
  {&OPEN-QUERY-br_titulos}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME desc_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL desc_cliente w-livre
ON F5 OF desc_cliente IN FRAME f-cad
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi_nom_abrev
                     &campozoom=nome_abrev}
                     
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_nom_abrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_nom_abrev w-livre
ON F5 OF fi_nom_abrev IN FRAME f-cad /* Nom.Abrev.Cliente */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi_nom_abrev
                     &campozoom=nome-abrev
                     &campozoom2=nome-emit
                     &campo2=desc_cliente}
                     
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_nom_abrev w-livre
ON LEAVE OF fi_nom_abrev IN FRAME f-cad /* Nom.Abrev.Cliente */
DO:
  FIND FIRST emitente
      WHERE emitente.nome-abrev = fi_nom_abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN
     ASSIGN desc_cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
  ELSE
     ASSIGN desc_cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'Cliente n∆o Encontrado'.
  IF fi_nom_abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = '' THEN
    ASSIGN desc_cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'Selecione um Cliente'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_nom_abrev w-livre
ON MOUSE-SELECT-DBLCLICK OF fi_nom_abrev IN FRAME f-cad /* Nom.Abrev.Cliente */
DO:
  APPLY 'f5'.
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


&Scoped-define SELF-NAME rs_tipo_pagamento
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs_tipo_pagamento w-livre
ON VALUE-CHANGED OF rs_tipo_pagamento IN FRAME f-cad
DO:
  CASE INPUT FRAME {&frame-name} rs_tipo_pagamento :
      WHEN 1 THEN  DO:
          ASSIGN FRAME fr_cheques:HIDDEN    = FALSE
                 FRAME fr_deposito:HIDDEN    = TRUE 
                 FRAME fr_antecipacao:HIDDEN = TRUE.
      END.
      WHEN 2 THEN DO:
          ASSIGN FRAME fr_cheques:HIDDEN      = TRUE
                 FRAME fr_deposito:HIDDEN     = FALSE
                 FRAME fr_antecipacao:HIDDEN  = TRUE.

      END.

      WHEN 3 THEN DO:
          ASSIGN FRAME fr_cheques:HIDDEN      = TRUE
                 FRAME fr_deposito:HIDDEN     = TRUE
                 FRAME fr_antecipacao:HIDDEN  = FALSE.

      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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
       RUN set-position IN h_p-exihel ( 1.13 , 94.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             sl_especie:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarEspecies w-livre 
PROCEDURE buscarEspecies :
DEFINE OUTPUT PARAMETER cListaEspecies       AS CHAR FORMAT 'x(4000)'.
DEFINE OUTPUT PARAMETER cListaEspeciesCodigo AS CHAR FORMAT 'x(4000)'.
ASSIGN cListaEspecies = 'TODAS,Todas'.
FOR EACH ems5.espec_docto:
    FIND FIRST espec_docto_financ_acr OF espec_docto
        NO-LOCK NO-ERROR.
    IF AVAIL espec_docto_financ_acr THEN DO:
        ASSIGN cListaEspecies = cListaEspecies + ',' +   espec_docto.cod_espec_docto + '-' + espec_docto.des_espec_docto + ',' + espec_docto.cod_espec_docto  .
        IF cListaEspeciesCodigo = '' THEN                                                                                                                               
           ASSIGN cListaEspeciesCodigo = espec_docto.cod_espec_docto.                                                                                                   
        ELSE                                                                                                                                                            
           ASSIGN cListaEspeciesCodigo = cListaEspeciesCodigo + ',' +  espec_docto.cod_espec_docto  .   
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarPortadores w-livre 
PROCEDURE buscarPortadores :
DEFINE OUTPUT PARAMETER cListaPortadores       AS CHAR FORMAT 'x(4000)'.
DEFINE OUTPUT PARAMETER cListaPortadoresCodigo AS CHAR FORMAT 'x(4000)'.

ASSIGN cListaPortadores = 'TODOS,Todos'.
FOR EACH ems5.portador:
    ASSIGN cListaPortadores = cListaPortadores + ',' +   portador.cod_portador + '-' + portador.nom_pessoa  + ',' + portador.cod_portador  .
    IF cListaPortadoresCodigo = '' THEN
       ASSIGN cListaPortadoresCodigo = portador.cod_portador.
    ELSE
       ASSIGN cListaPortadoresCodigo = cListaPortadoresCodigo + ',' +   portador.cod_portador  .



END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarTitulosFiltro w-livre 
PROCEDURE buscarTitulosFiltro :
DEFINE INPUT  PARAMETER p_cod_estab         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER p_cdn_cliente       AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_espec_docto   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_portador      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER p_dt_emissao_ini    AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER p_dt_emissao_fim    AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER p_dt_vencto_ini     AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER p_dt_vencto_fim     AS DATE        NO-UNDO.
DEFINE VARIABLE cListaPortador AS CHARACTER   NO-UNDO FORMAT 'x(2000)'.
IF p_cod_portador = 'todos' THEN 
   ASSIGN p_cod_portador = cListaPortadoresCodigo.
ELSE
   ASSIGN p_cod_portador = sl_portador:SCREEN-VALUE IN FRAME {&FRAME-NAME}.


IF p_cod_espec_docto = 'todas' THEN 
   ASSIGN p_cod_espec_docto = cListaEspeciesCodigo.
ELSE
   ASSIGN p_cod_espec_docto = sl_especie:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

/*MESSAGE 'p_cod_estab     '  p_cod_estab     SKIP             
        'p_cdn_cliente   '  p_cdn_cliente   SKIP         
        ' portador       '  p_cod_portador  SKIP        
        ' espec_docto    '  p_cod_espec_docto    SKIP        
        'YES             '  YES             SKIP        
        'p_dt_emissao_ini'  p_dt_emissao_ini SKIP        
        'p_dt_emissao_fim'  p_dt_emissao_fim SKIP       
        'p_dt_vencto_ini '  p_dt_vencto_ini  SKIP       
        'p_dt_vencto_fim '  p_dt_vencto_fim SKIP        


    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
 

FOR EACH tit_acr NO-LOCK 
    WHERE tit_acr.cod_estab                                  = p_cod_estab
    AND   tit_acr.cdn_cliente                                = p_cdn_cliente
    AND   LOOKUP(tit_acr.cod_portador,p_cod_portador)        > 0    
    AND   LOOKUP(tit_acr.cod_espec_docto,p_cod_espec_docto ) > 0
    AND   tit_acr.log_sdo_tit_acr                            = YES
    AND   tit_acr.dat_emis_docto                            >= p_dt_emissao_ini
    AND   tit_acr.dat_emis_docto                            <= p_dt_emissao_fim
    AND   tit_acr.dat_vencto_tit_acr                        >= p_dt_vencto_ini 
    AND   tit_acr.dat_vencto_tit_acr                        <= p_dt_vencto_fim 
    USE-INDEX titacr_cliente:
    CREATE tt_titulos.
    ASSIGN tt_titulos.cod_estab         = tit_acr.cod_estab
           tt_titulos.cod_espec_docto   = tit_acr.cod_espec_docto
           tt_titulos.cod_portador      = tit_acr.cod_portador
           tt_titulos.cdn_cliente       = tit_acr.cdn_cliente
           tt_titulos.cod_ser_docto     = tit_acr.cod_ser_docto
           tt_titulos.cod_tit_acr       = tit_acr.cod_tit_acr
           tt_titulos.cod_parcela       = tit_acr.cod_parcela
           tt_titulos.cod_portador      = tit_acr.cod_portador
           tt_titulos.val_sdo_tit_acr   = tit_acr.val_sdo_tit_acr
           tt_titulos.val_multa_tit_acr = tit_acr.val_multa_tit_acr
           tt_titulos.val_despes_financ = tit_acr.val_despes_financ
           tt_titulos.val_despes_bcia   = tit_acr.val_despes_bcia.
END.
                                                                  
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
  DISPLAY sl_especie sl_portador dt_vencto_ini dt_vencto_fim fi_nom_abrev 
          desc_cliente dt_emissao_fim dt_emissao_ini dt_base dt_media_titulos 
          dt_media_pagtos rs_tipo_pagamento fi_tot_titulos fi_tot_cheque 
          fi_tot_deposito fi_tot_antecipacao fi_saldo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 sl_especie sl_portador dt_vencto_ini dt_vencto_fim 
         fi_nom_abrev bt-efetivar dt_emissao_fim dt_emissao_ini bt_filtrar 
         dt_base br_titulos dt_media_titulos dt_media_pagtos rs_tipo_pagamento 
         bt-efetivar-2 fi_tot_titulos fi_tot_cheque fi_tot_deposito 
         fi_tot_antecipacao fi_saldo 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  ENABLE BROWSE-2 
      WITH FRAME fr_antecipacao IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-fr_antecipacao}
  ENABLE br_cheques 
      WITH FRAME fr_cheques IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-fr_cheques}
  ENABLE br_depositos 
      WITH FRAME fr_deposito IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-fr_deposito}
  VIEW w-livre.
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

  {utp/ut9000.i "esacr001" "11.05.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  RUN buscarPortadores(OUTPUT cListaPortadores, OUTPUT cListaPortadoresCodigo).
  RUN buscarEspecies(OUTPUT cListaEspecies, OUTPUT cListaEspeciesCodigo).
  ASSIGN sl_portador:LIST-ITEM-PAIRS = cListaPortadores
         sl_especie:LIST-ITEM-PAIRS  = cListaEspecies.
  ASSIGN sl_especie:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'todas'
         sl_portador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'todos'
         dt_base:SCREEN-VALUE = STRING(TODAY).
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN FRAME fr_cheques:HIDDEN    = FALSE
         FRAME fr_deposito:HIDDEN    = TRUE 
         FRAME fr_antecipacao:HIDDEN = TRUE.


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
  {src/adm/template/snd-list.i "tt_titulos"}
  {src/adm/template/snd-list.i "tt_antecipacoes"}
  {src/adm/template/snd-list.i "tt_integr_acr_cheq"}
  {src/adm/template/snd-list.i "tt_integr_acr_liq_item_lote_3"}

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

