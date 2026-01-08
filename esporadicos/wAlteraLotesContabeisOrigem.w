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

DEFINE TEMP-TABLE tt
    FIELD tipo   AS CHAR
    FIELD conta  AS CHAR FORMAT 'x(8)'
    FIELD cCusto AS CHAR
    FIELD valor  AS DECIMAL
    FIELD rROWID AS ROWID 
    FIELD cod_MODULO AS CHAR.

DEFINE VARIABLE rowidCorrente AS ROWID       NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt.tipo tt.conta tt.ccusto tt.valor tt.cod_modulo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiNumLote fiNumLancto fiNumSeqLancto ~
btExecutar BROWSE-2 fictaCtbl ficcusto btAlterar rt-button RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fiNumLote fiNumLancto fiNumSeqLancto ~
fictaCtbl fidescCta ficcusto fidescCCusto 

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
DEFINE BUTTON btAlterar 
     LABEL "Alterar" 
     SIZE 14 BY 1.13.

DEFINE BUTTON btExecutar 
     LABEL "Buscar" 
     SIZE 15.86 BY 1.13.

DEFINE VARIABLE ficcusto AS CHARACTER FORMAT "X(5)":U 
     LABEL "Centro de Custo" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .79 NO-UNDO.

DEFINE VARIABLE fictaCtbl AS CHARACTER FORMAT "X(8)":U 
     LABEL "Conta Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .79 NO-UNDO.

DEFINE VARIABLE fidescCCusto AS CHARACTER FORMAT "X(108)":U 
     VIEW-AS FILL-IN 
     SIZE 57.43 BY .79 NO-UNDO.

DEFINE VARIABLE fidescCta AS CHARACTER FORMAT "X(108)":U 
     VIEW-AS FILL-IN 
     SIZE 57.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiNumLancto AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Lanáamento Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 6.72 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumLote AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Lote Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumSeqLancto AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Sequància Lanáto" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.29 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt.tipo    COLUMN-LABEL "Tipo"        
 tt.conta   COLUMN-LABEL "Conta"  FORMAT 'x(12)'
 tt.ccusto  COLUMN-LABEL "C.Custo"
 tt.valor   COLUMN-LABEL "Valor"
 tt.cod_modulo COLUMN-LABEL "Modulo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89 BY 7.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiNumLote AT ROW 2.75 COL 10 COLON-ALIGNED HELP
          "N£mero Lote Cont†bil" WIDGET-ID 4
     fiNumLancto AT ROW 2.75 COL 36.29 COLON-ALIGNED HELP
          "N£mero Lanáamento Cont†bil" WIDGET-ID 2
     fiNumSeqLancto AT ROW 2.75 COL 58.43 COLON-ALIGNED HELP
          "N£mero SeqÅància Lanáamento" WIDGET-ID 6
     btExecutar AT ROW 2.71 COL 67 WIDGET-ID 8
     BROWSE-2 AT ROW 4.5 COL 2 WIDGET-ID 200
     fictaCtbl AT ROW 12.5 COL 12 COLON-ALIGNED WIDGET-ID 12
     fidescCta AT ROW 12.5 COL 29.57 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     ficcusto AT ROW 13.5 COL 12 COLON-ALIGNED WIDGET-ID 14
     fidescCCusto AT ROW 13.5 COL 29.72 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     btAlterar AT ROW 14.63 COL 14 WIDGET-ID 16
     "IMPORTANTE: ê necess†rio contabilizar o modulo novamente, ap¢s, a alteraá∆o." VIEW-AS TEXT
          SIZE 88 BY 1.5 AT ROW 16.71 COL 1.86 WIDGET-ID 22
          FGCOLOR 12 FONT 0
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.5 COL 1.86 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.25
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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 17.25
         WIDTH              = 90.29
         MAX-HEIGHT         = 17.25
         MAX-WIDTH          = 92.86
         VIRTUAL-HEIGHT     = 17.25
         VIRTUAL-WIDTH      = 92.86
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-2 btExecutar f-cad */
/* SETTINGS FOR FILL-IN fidescCCusto IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fidescCCusto:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fidescCta IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fidescCta:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
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


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON VALUE-CHANGED OF BROWSE-2 IN FRAME f-cad
DO:
  FIND FIRST cta_ctbl
      WHERE cta_ctbl.cod_cta_ctbl = tt.conta:SCREEN-VALUE IN BROWSE {&browse-NAME}
      NO-LOCK NO-ERROR.
  FIND FIRST ems5.cCusto
      WHERE ccusto.cod_ccusto = tt.cCusto:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-LOCK NO-ERROR.

  ASSIGN fiCtaCtbl:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = IF AVAIL cta_ctbl THEN cta_ctbl.cod_cta_ctbl    ELSE ''
         fiDescCta:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = IF AVAIL cta_ctbl THEN cta_ctbl.des_tit_ctbl    ELSE ''
         ficCusto:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = IF AVAIL cCusto   THEN ems5.ccusto.cod_cCusto   ELSE ''
         fiDescCCusto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL cCusto   THEN ems5.ccusto.des_tit_ctbl ELSE ''.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAlterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAlterar w-livre
ON CHOOSE OF btAlterar IN FRAME f-cad /* Alterar */
DO:
  DISABLE TRIGGERS FOR LOAD OF aprop_ctbl_ap.
  DISABLE TRIGGERS FOR LOAD OF aprop_ctbl_acr.
  DISABLE TRIGGERS FOR LOAD OF aprop_ctbl_cmg.
     
  CASE INPUT BROWSE {&browse-name} tt.cod_MODULO:
      WHEN 'apb' THEN DO:
          FIND FIRST aprop_ctbl_ap
              WHERE ROWID(aprop_ctbl_ap) = tt.rRowid
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL aprop_ctbl_ap THEN DO:
             ASSIGN aprop_ctbl_ap.cod_cta_ctbl = fiCtaCtbl:SCREEN-VALUE
                    aprop_ctbl_ap.cod_ccusto   = ficCusto:SCREEN-VALUE.
             IF aprop_ctbl_ap.cod_cCusto <> '' AND aprop_ctbl_ap.cod_plano_ccusto  = '' THEN DO:
                CASE lancto_ctbl.cod_empresa:
                    WHEN '1' THEN
                        ASSIGN aprop_ctbl_ap.cod_plano_ccusto = 'IMA'.
                    WHEN '5' THEN
                        ASSIGN aprop_ctbl_ap.cod_plano_ccusto = 'MED'.
                    OTHERWISE
                        MESSAGE 'Empresa N∆o Configurada para atribuiá∆o automatica de plano de centro de custo!!!'
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END CASE.
             END.
          END.
      END.
      WHEN 'acr' THEN DO:
          FIND FIRST aprop_ctbl_acr
              WHERE ROWID(aprop_ctbl_acr) = tt.rRowid
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL aprop_ctbl_acr THEN DO:
             ASSIGN aprop_ctbl_acr.cod_cta_ctbl = fiCtaCtbl:SCREEN-VALUE
                    aprop_ctbl_acr.cod_ccusto   = ficCusto:SCREEN-VALUE.
             IF aprop_ctbl_acr.cod_cCusto <> '' AND aprop_ctbl_acr.cod_plano_ccusto  = '' THEN DO:
                CASE lancto_ctbl.cod_empresa:
                    WHEN '1' THEN
                        ASSIGN aprop_ctbl_acr.cod_plano_ccusto = 'IMA'.
                    WHEN '5' THEN
                        ASSIGN aprop_ctbl_acr.cod_plano_ccusto = 'MED'.
                    OTHERWISE
                        MESSAGE 'Empresa N∆o Configurada para atribuiá∆o automatica de plano de centro de custo!!!'
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END CASE.
             END.
          END.
      END.
      WHEN 'cmg' THEN DO:
          FIND FIRST aprop_ctbl_cmg
              WHERE ROWID(aprop_ctbl_cmg) = tt.rRowid
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL aprop_ctbl_cmg THEN DO:
             ASSIGN aprop_ctbl_cmg.cod_cta_ctbl = fiCtaCtbl:SCREEN-VALUE
                    aprop_ctbl_cmg.cod_ccusto   = ficCusto:SCREEN-VALUE.
             IF aprop_ctbl_cmg.cod_cCusto <> '' AND aprop_ctbl_cmg.cod_plano_ccusto  = '' THEN DO:
                CASE lancto_ctbl.cod_empresa:
                    WHEN '1' THEN
                        ASSIGN aprop_ctbl_cmg.cod_plano_ccusto = 'IMA'.
                    WHEN '5' THEN
                        ASSIGN aprop_ctbl_cmg.cod_plano_ccusto = 'MED'.
                    OTHERWISE
                        MESSAGE 'Empresa N∆o Configurada para atribuiá∆o automatica de plano de centro de custo!!!'
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
                END CASE.
             END.
          END.
      END.
  END CASE .
  APPLY 'choose' TO btExecutar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecutar w-livre
ON CHOOSE OF btExecutar IN FRAME f-cad /* Buscar */
DO:
  EMPTY TEMP-TABLE tt.
  FIND FIRST ITEM_lancto_ctbl
      WHERE  ITEM_lancto_ctbl.num_lote_ctbl         = INPUT FRAME {&frame-name} fiNumLote
      AND    ITEM_lancto_ctbl.num_lancto_ctbl       = INPUT FRAME {&frame-name} fiNumLancto
      AND    ITEM_lancto_ctbl.num_seq_lancto_ctbl   = INPUT FRAME {&FRAME-NAME} fiNumSeqLancto
      NO-LOCK NO-ERROR.
  IF AVAIL ITEM_lancto_ctbl THEN DO:
     /*MESSAGE 'achei item_lancto_ctbl'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
     FIND FIRST lancto_ctbl OF ITEM_lancto_ctbl NO-LOCK NO-ERROR.
     FOR EACH aprop_lancto_ctbl OF ITEM_lancto_ctbl NO-LOCK.
         /*MESSAGE 'achei aprop_lancto_ctbl'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
         CASE lancto_ctbl.cod_modul_dtsul:
             WHEN 'acr' THEN DO:
                 /*MESSAGE 'acr'
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                 FOR EACH val_aprop_ctbl_acr OF aprop_lancto_ctbl
                     WHERE val_aprop_ctbl_acr.cod_finalid_econ = 'corrente' NO-LOCK:
                     FOR EACH aprop_ctbl_acr OF val_aprop_ctbl_acr
                          NO-LOCK:
                         CREATE tt.
                         ASSIGN tt.conta  = aprop_ctbl_acr.cod_cta_ctbl
                                tt.cCusto = aprop_ctbl_acr.cod_cCusto
                                tt.tipo   = aprop_ctbl_acr.ind_natur_lancto_ctbl
                                tt.valor  = aprop_ctbl_acr.val_aprop_ctbl 
                                tt.rRowid = ROWID(aprop_ctbl_acr)
                                tt.cod_MODULO = 'ACR'.
                     END.
                 END.
             END.
             WHEN 'apb' THEN DO:
                 /*MESSAGE 'apb'
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                 FOR EACH val_aprop_ctbl_ap OF aprop_lancto_ctbl
                     WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK:
                     MESSAGE 'achou val_aprop_ctbl_ap'
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
                     FOR EACH aprop_ctbl_ap OF val_aprop_ctbl_ap NO-LOCK:
                         CREATE tt.
                         ASSIGN tt.conta  = aprop_ctbl_ap.cod_cta_ctbl
                                tt.cCusto = aprop_ctbl_ap.cod_cCusto
                                tt.tipo   = aprop_ctbl_ap.ind_natur_lancto_ctbl
                                tt.valor  = aprop_ctbl_ap.val_aprop_ctbl
                                tt.rRowid = ROWID(aprop_ctbl_ap)
                                tt.cod_MODULO = 'APB'.
                     END.
                 END.
             END.
             WHEN 'cmg' THEN DO:
                 /*MESSAGE 'cmg'
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                 FOR EACH val_aprop_ctbl_cmg OF aprop_lancto_ctbl
                     WHERE val_aprop_ctbl_cmg.cod_finalid_econ = 'corrente' NO-LOCK:
                     FOR EACH aprop_ctbl_cmg OF val_aprop_ctbl_cmg NO-LOCK:
                         CREATE tt.
                         ASSIGN tt.conta  = aprop_ctbl_cmg.cod_cta_ctbl
                                tt.cCusto = aprop_ctbl_cmg.cod_cCusto
                                tt.tipo   = aprop_ctbl_cmg.ind_natur_lancto_ctbl 
                                tt.valor  = aprop_ctbl_cmg.val_movto_cta_corren
                                tt.rRowid = ROWID(aprop_ctbl_cmg)
                                tt.cod_MODULO = 'CMG'.
                     END.
                 END.                                                             
             END.
             OTHERWISE
                 MESSAGE 'Esse Lanáamento n∆o tem origem nos m¢dulos:CMG,APB,ACR !!!'
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
         END CASE.
     END.
  END.
  ELSE DO:
     MESSAGE 'Registro n∆o encontrado'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
  END.
  {&OPEN-QUERY-BROWSE-2}
  APPLY 'value-changed' TO BROWSE {&browse-name}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ficcusto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficcusto w-livre
ON LEAVE OF ficcusto IN FRAME f-cad /* Centro de Custo */
DO:
  FIND FIRST ems5.ccusto
      WHERE ccusto.cod_ccusto = ficcusto:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  ASSIGN fidescCCusto:SCREEN-VALUE = IF AVAIL ccusto THEN cCusto.des_tit_ctbl ELSE ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fictaCtbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fictaCtbl w-livre
ON LEAVE OF fictaCtbl IN FRAME f-cad /* Conta Cont†bil */
DO:
   FIND FIRST cta_ctbl
      WHERE cta_ctbl.cod_cta_ctbl = fiCtaCtbl:SCREEN-VALUE
      NO-LOCK NO-ERROR.
   ASSIGN fiDescCta:SCREEN-VALUE = IF AVAIL cta_ctbl THEN cta_ctbl.des_tit_ctbl ELSE ''.
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
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiNumLote:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fiNumLote fiNumLancto fiNumSeqLancto fictaCtbl fidescCta ficcusto 
          fidescCCusto 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE fiNumLote fiNumLancto fiNumSeqLancto btExecutar BROWSE-2 fictaCtbl 
         ficcusto btAlterar rt-button RECT-1 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
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

  {utp/ut9000.i "XX9999" "9.99.99.999"}

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
  {src/adm/template/snd-list.i "tt"}

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

