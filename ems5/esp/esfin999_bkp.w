&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i esfin999 12.01.07.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cbaseOutra          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE cbaseAtual          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE cempresaAtual       AS CHARACTER   NO-UNDO INIT '5'.
DEFINE VARIABLE arquivoSaida        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAmbiente           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConexao            AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE digitoAmbiente      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE servidorAmbiente    AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE dbems5              AS CHARACTER   NO-UNDO FORMAT 'x(10)'.
DEFINE VARIABLE cBaseDesconectar    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPortaDesconectar   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dValor              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE valorInd            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dIndice             AS DECIMAL     NO-UNDO.

DEFINE VARIABLE hLog                AS HANDLE      NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPlanilha           AS CHARACTER   NO-UNDO FORMAT 'x(120)'.


DEFINE TEMP-TABLE tt
    FIELD partida                   AS CHAR
    FIELD origem                    AS CHAR FORMAT 'x(100)'
    FIELD cod_empresa               AS CHAR
    FIELD cod_estab                 AS CHAR
    FIELD cod_emitente              AS CHAR FORMAT 'x(50)'
    FIELD desc_emitente             AS CHAR FORMAT 'x(200)'
    FIELD data                      AS DATE
    FIELD nro_docto                 AS CHAR FORMAT 'x(20)'
    FIELD serie                     AS CHAR FORMAT 'x(20)'
    FIELD parcela                   AS CHAR
    FIELD especie                   AS CHAR
    FIELD cod_refer                 AS CHAR
    /*FIELD cod_classif               AS INT
    FIELD desc_classif              AS CHAR FORMAT 'x(50)'*/
    FIELD conta_contabil            AS CHAR FORMAT 'x(12)'
    FIELD valor                     AS DECIMAL  
    FIELD conta_corrente            AS CHAR FORMAT 'x(20)'
    FIELD base                      AS CHAR 
    FIELD DESC_conta                AS CHAR FORMAT 'x(150)'
    FIELD tipo                      AS CHAR
    FIELD cc                        AS CHAR 
    FIELD DESC_cc                   AS CHAR FORMAT 'x(50)'
    FIELD rowidNota                 AS ROWID
    FIELD id_movto_corren           AS INT
    FIELD sequencia                 AS INT
    FIELD grupo_emitente            AS CHAR
    FIELD classificacao             AS CHAR FORMAT 'x(50)'
    FIELD grupo                     AS CHAR FORMAT 'x(50)'
    FIELD cCusto_Gerencial          AS CHAR FORMAT 'x(50)'
    FIELD cod_modulo                AS CHAR
    FIELD cod_param_desemb          AS INT
    FIELD cod_param_desemb_cCusto   AS INT
    FIELD LOG_desconsiderar         AS LOGICAL FORMAT "sim/n∆o"
    FIELD historico                 AS CHAR FORMAT 'x(2000)'
    FIELD transacao                 AS CHAR .

DEFINE BUFFER bf_movto_tit_ap FOR movto_tit_ap.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button fiDtIni fiDtFim tg_compensacao_cr ~
tg_compensacao_ap btExecutar tglog 
&Scoped-Define DISPLAYED-OBJECTS fiDtIni fiDtFim tg_compensacao_cr ~
tg_compensacao_ap 

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
DEFINE BUTTON btExecutar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiDtFim AS DATE FORMAT "99/99/9999":U 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDtIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Data De" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tglog AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.29 BY .83 NO-UNDO.

DEFINE VARIABLE tg_compensacao_ap AS LOGICAL INITIAL no 
     LABEL "Compensaá‰es Contas a Pagar" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .83 NO-UNDO.

DEFINE VARIABLE tg_compensacao_cr AS LOGICAL INITIAL yes 
     LABEL "Compensaá‰es Contas a Receber" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiDtIni AT ROW 2.75 COL 13.72 COLON-ALIGNED WIDGET-ID 4
     fiDtFim AT ROW 2.75 COL 34 COLON-ALIGNED WIDGET-ID 6
     tg_compensacao_cr AT ROW 4.13 COL 16 WIDGET-ID 10
     tg_compensacao_ap AT ROW 5.04 COL 16 WIDGET-ID 12
     btExecutar AT ROW 6.25 COL 15 WIDGET-ID 2
     tglog AT ROW 6.5 COL 31 WIDGET-ID 8
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
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
         HEIGHT             = 6.5
         WIDTH              = 59.14
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 92.57
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 92.57
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
/* SETTINGS FOR TOGGLE-BOX tglog IN FRAME f-cad
   NO-DISPLAY                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME btExecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecutar w-livre
ON CHOOSE OF btExecutar IN FRAME f-cad /* Executar */
DO:
 ASSIGN btExecutar:LABEL = "Executando.."
        btExecutar:SENSITIVE    = NO.
 ASSIGN arquivoSaida            = 'esfin999.txt'.
 
OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + ArquivoSaida).
/*limpar arquivo*/
OUTPUT CLOSE.

RUN buscarMovtos.

FOR EACH tt BREAK BY tt.conta_contabil:
    FIND FIRST ems5.cta_ctbl
           WHERE cta_ctbl.cod_cta_ctbl = tt.conta_contabil NO-LOCK NO-ERROR.
    ASSIGN tt.DESC_conta = IF AVAIL  cta_ctbl THEN cta_ctbl.DES_tit_ctbl ELSE 'Conta n∆o Encontrada'.
END.

IF INPUT FRAME  {&frame-name} tg_compensacao_cr  THEN DO:
   RUN buscarCompensacoesCR.
END.


OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + ArquivoSaida).
FOR EACH tt:
    EXPORT DELIMITER "|" tt EXCEPT tt.rowidnota 
        tt.cod_param_desemb          
        tt.cod_param_desemb_cCusto   
        tt.LOG_desconsiderar         
        tt.classificacao
        tt.ccusto_gerencial
        tt.grupo
        tt.base.
END.
OUTPUT CLOSE.




ASSIGN cPlanilha = SEARCH("excel\lanctos_contabeis_cx.xlsx").
OS-COMMAND SILENT VALUE(" start excel /t " + cPlanilha) .
ASSIGN btExecutar:LABEL = "Executar"
       btExecutar:SENSITIVE = YES.

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
       RUN set-position IN h_p-exihel ( 1.13 , 44.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiDtIni:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarCompensacoesCR w-livre 
PROCEDURE buscarCompensacoesCR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hBoCompensacaoTitAcr   AS HANDLE      NO-UNDO.
RUN esbo\bo_compensacao_tit_acr.p PERSISTENT SET  hBoCompensacaoTitAcr.
RUN setDataTransacao  IN hBoCompensacaoTitAcr(INPUT FRAME {&FRAME-NAME} fiDtIni, INPUT FRAME {&FRAME-NAME} fiDtFim).
RUN buscarRegistros   IN hBoCompensacaoTitAcr.
RUN exportarRegistros IN hBoCompensacaoTitAcr('c:\temp\compensacoesCR.txt').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarMovtos w-livre 
PROCEDURE buscarMovtos :
/**************************************************************************
busca todos os movimentos de caixas e bancos conforme filtros da tela.
**************************************************************************/

DEFINE VARIABLE iCont    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cContaContabil AS CHARACTER   NO-UNDO FORMAT 'x(30)'.
FOR EACH movto_cta_corren 
    WHERE movto_cta_corren.dat_transacao >= INPUT FRAME {&frame-name} fiDtIni
    AND   movto_cta_corren.dat_transacao <= INPUT FRAME {&frame-name} fiDtFim
    AND   ind_tip_movto_cta_corren = 're'
    NO-LOCK:
    
    FIND FIRST cta_corren OF movto_cta_corren NO-LOCK NO-ERROR.    
    CASE movto_cta_corren.cod_modul_dtsul:
        WHEN 'acr'  THEN DO:
           FOR EACH movto_tit_acr OF movto_cta_corren NO-LOCK:
               FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.                                                                                                               
               FIND FIRST emitente WHERE                                                                                                                                           
                    tit_acr.cdn_cliente =  emitente.cod-emitente NO-LOCK NO-ERROR. 
               FIND FIRST ext-emitente OF emitente
                   NO-LOCK NO-ERROR.
               IF AVAIL ext-emitente THEN
                  FIND FIRST ramo-ativ
                       WHERE ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
               FOR EACH aprop_ctbl_acr OF movto_tit_acr                                                                                                                               
                   /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr')                                                       
                   OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db')*/  NO-LOCK :   
                   RUN criarTT( aprop_ctbl_acr.ind_natur_lancto_ctbl , aprop_ctbl_acr.cod_empresa , aprop_ctbl_acr.cod_estab , IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'  ,
                       IF AVAIL emitente THEN emitente.nome-emit ELSE '', movto_cta_corren.dat_transacao , aprop_ctbl_acr.cod_cta_ctbl  , aprop_ctbl_acr.val_aprop_ctbl ,
                       movto_cta_corren.cod_cta_corren, 'Contas a Receber', movto_cta_corren.cod_modul_dtsul , movto_cta_corren.ind_fluxo_movto_cta_corren ,
                       aprop_ctbl_acr.cod_ccusto , tit_acr.cod_tit_acr , tit_acr.Cod_ser_docto , tit_acr.cod_parcela  , tit_acr.cod_espec_docto ,
                       movto_tit_acr.cod_refer, movto_cta_corren.num_seq_movto_cta_corren , movto_cta_corren.num_id_movto_cta_corren , 
                        IF AVAIL ramo-ativ THEN string(ramo-ativ.cod-ramo-ativ) + '-' + ramo-ativ.descricao ELSE '' , 
                        replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";") , movto_tit_acr.ind_trans_acr_abrev).

               END.
           END.
        END.
        WHEN 'apb'  THEN DO:
           FOR EACH movto_tit_ap OF movto_cta_corren NO-LOCK:
               FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR. 
               FIND FIRST EMS5.espec_docto OF tit_ap NO-LOCK NO-ERROR.
               FIND FIRST EMS5.fornecedor                                                                                                                                              
                WHERE tit_ap.cdn_fornecedor = fornecedor.cdn_fornecedor NO-LOCK NO-ERROR.
               IF AVAIL fornecedor THEN
                  FIND FIRST grp_fornec where
                   grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
               IF NOT AVAIL tit_Ap  THEN DO:
                   /*caso n∆o exista o titulo, faz o tratamento para PEF*/
                   FOR EACH ems5.antecip_pef_pend OF movto_tit_ap  NO-LOCK:
                       FIND FIRST ems5.fornecedor OF ems5.antecip_pef_pend NO-LOCK NO-ERROR.
                       IF AVAIL fornecedor THEN
                       FIND FIRST ems5.grp_fornec where
                             ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
                       FOR EACH ems5.aprop_ctbl_pend_ap OF antecip_pef_pend NO-LOCK:
                           RUN criarTT('db', aprop_ctbl_pend_ap.cod_empresa, aprop_ctbl_pend_ap.cod_estab, string(ems5.antecip_pef_pend.cdn_fornecedor),
                                 IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE '', movto_cta_corren.dat_transacao, aprop_ctbl_pend_ap.cod_cta_ctbl ,
                                 aprop_ctbl_pend_ap.val_aprop_ctbl   , movto_cta_corren.cod_cta_corren , 'Contas a Pagar - PEF', movto_cta_corren.cod_modul_dtsul,
                                 movto_cta_corren.ind_fluxo_movto_cta_corren, aprop_ctbl_pend_ap.cod_ccusto,antecip_pef_pend.cod_tit_ap, antecip_pef_pend.cod_ser_docto,         
                                 antecip_pef_pend.cod_parcela, antecip_pef_pend.cod_espec_docto, antecip_pef_pend.cod_refer, movto_cta_corren.num_seq_movto_cta_corren,       
                                 movto_cta_corren.num_id_movto_cta_corren, IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE '',
                                 REPLACE(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),    
                                 movto_tit_ap.ind_trans_ap_abrev ).
                           RUN retornarCtaPortador(antecip_pef_pend.cod_portador, antecip_pef_pend.cod_finalid_econ, OUTPUT cContaContabil).
                           RUN criarTT('CR', aprop_ctbl_pend_ap.cod_empresa, aprop_ctbl_pend_ap.cod_estab, string(ems5.antecip_pef_pend.cdn_fornecedor),
                                 IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE '', movto_cta_corren.dat_transacao, cContaContabil ,
                                 aprop_ctbl_pend_ap.val_aprop_ctbl   , movto_cta_corren.cod_cta_corren , 'Contas a Pagar - PEF', movto_cta_corren.cod_modul_dtsul,
                                 movto_cta_corren.ind_fluxo_movto_cta_corren, '',antecip_pef_pend.cod_tit_ap, antecip_pef_pend.cod_ser_docto,         
                                 antecip_pef_pend.cod_parcela, antecip_pef_pend.cod_espec_docto, antecip_pef_pend.cod_refer, movto_cta_corren.num_seq_movto_cta_corren,       
                                 movto_cta_corren.num_id_movto_cta_corren, IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE '',
                                 REPLACE(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),    
                                 movto_tit_ap.ind_trans_ap_abrev ).

                           
                       END.
                    END.
                    /** criaá∆o do registro de credito a partir do de debito **/
                  
               END.
               ELSE DO:
                  /**************************************************************************************************
                     verifica a diferenca de valor entre a a baixa e a implantacao e cria indice proporcional                                                     
                      para tratar valores retirados ou acrescidos ap¢s a implantacao do titulo
                   **************************************************************************************************/      
                   

                   FIND FIRST bf_movto_tit_ap OF tit_ap                                                                                                         
                       WHERE bf_movto_tit_ap.ind_trans_ap_abrev = 'IMPL'                                                                                        
                       OR    bf_movto_tit_ap.ind_trans_ap_abrev  = 'SBND'  NO-LOCK NO-ERROR.                                                                   

                     
                   ASSIGN  valorInd = movto_tit_ap.val_movto_ap + movto_tit_ap.val_juros - movto_tit_ap.val_desconto.
                   IF bf_movto_tit_ap.val_movto_ap <> valorInd THEN DO:                 
                      ASSIGN dIndice = bf_movto_tit_ap.val_movto_ap / valorInd.         
                   END.                                                                                                                                         
                   ELSE                                                                                                                                         
                      ASSIGN dIndice = 1. 

                   /*MESSAGE 'existe o tit ap' SKIP
                           dindice
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


                   /*********************************************************************/
                  FOR EACH ems5.aprop_ctbl_ap OF movto_tit_ap NO-LOCK                                                                                                                   
                           /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
                            OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db') */ :
                     /* MESSAGE 'aprop ctbl'
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                          FIND tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.
                          IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
                              FIND FIRST ems5.val_aprop_ctbl_ap OF aprop_ctbl_ap
                                    WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
                              IF AVAIL val_aprop_ctbl_ap THEN 
                                 ASSIGN dvalor = val_aprop_ctbl_ap.val_aprop_ctbl / dIndice.
                              ELSE
                                 ASSIGN dvalor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
                           END.
                           ELSE DO:
                               ASSIGN dvalor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
                           END.
                           ASSIGN icont = icont + 1.
                           RUN criarTT(aprop_ctbl_ap.ind_natur_lancto_ctbl, aprop_ctbl_ap.cod_empresa, aprop_ctbl_ap.cod_estab, string(tit_ap.cdn_fornecedor), 
                                       IF AVAIL fornecedor THEN ems5.fornecedor.nom_pessoa  ELSE '', movto_cta_corren.dat_movto_cta_corren,aprop_ctbl_ap.cod_cta_ctbl,
                                       dValor, movto_cta_corren.cod_cta_corren,'Contas a Pagar - Titulo', 'APB', movto_cta_corren.ind_fluxo_movto_cta_corren, aprop_ctbl_ap.cod_ccusto,
                                 tit_ap.cod_tit_ap,tit_ap.cod_ser_docto, tit_ap.cod_parcela, tit_ap.cod_espec_docto, movto_tit_ap.cod_refer, movto_cta_corren.num_seq_movto_cta_corren,
                                 movto_cta_corren.num_id_movto_cta_corren, IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE '',
                                 replace(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"), movto_tit_ap.ind_trans_ap_abrev ).                                                                                         
                  END.
                    
               END.
           END.
        END.
        WHEN 'cmg'  THEN DO:
            FIND FIRST tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
            FOR EACH aprop_ctbl_cmg OF movto_cta_corren
                /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
                OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK */ :
               RUN criarTT(aprop_ctbl_cmg.ind_natur_lancto_ctbl,aprop_ctbl_cmg.cod_empresa,aprop_ctbl_cmg.cod_estab,movto_cta_corren.cod_tip_trans_cx,
               IF AVAIL tip_trans_cx THEN tip_trans_cx.des_tip_trans_cx  ELSE '',movto_cta_corren.dat_transacao,aprop_ctbl_cmg.cod_cta_ctbl,aprop_ctbl_cmg.val_movto_cta_corren,
               movto_cta_corren.cod_cta_corren,'Caixa e Bancos',movto_cta_corren.cod_modul_dtsul,movto_cta_corren.ind_fluxo_movto_cta_corren,aprop_ctbl_cmg.cod_ccusto,                                                               
               cod_docto_movto_cta_bco,'','','','',movto_cta_corren.num_seq_movto_cta_corren,movto_cta_corren.num_id_movto_cta_corren,'',                                                                                    
                replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),'') .
            END.
        END.
    END CASE.
END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criarTT w-livre 
PROCEDURE criarTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pPartida        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEmpresa     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEstab       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEmitente    AS CHAR        NO-UNDO.
DEFINE INPUT  PARAMETER pDescEmitente   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pContaContabil  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pValor          AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pContaCorrente  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pOrigem         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodModulo      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pTipo           AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCC             AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pnroDocto       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pParcela        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pEspecie        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER PCodRefer       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSequencia      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pIdMovtoCorren  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pGrupoEmitente  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pHistorico      AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE INPUT  PARAMETER pTransacao      AS CHARACTER   NO-UNDO.


CREATE tt.             
ASSIGN                 
tt.partida           = pPartida        
tt.cod_modulo        = pCodModulo  
tt.cod_empresa       = pCodEmpresa            
tt.cod_estab         = pCodEstab    
tt.cod_emitente      = pCodEmitente   
tt.desc_emitente     = pDescEmitente           
tt.data              = pData  
tt.conta_contabil    = pContaContabil           
tt.valor             = pValor  
tt.conta_corrente    = pContaCorrente         
tt.origem            = pOrigem     
tt.tipo              = pTipo           
tt.cc                = pCC             
tt.nro_docto         = pnroDocto       
tt.serie             = pSerie          
tt.parcela           = pParcela        
tt.especie           = pEspecie        
tt.cod_refer         = PCodRefer       
tt.sequencia         = pSequencia      
tt.id_movto_corren   = pIdMovtoCorren  
tt.grupo_emitente    = pGrupoEmitente  
tt.historico         = pHistorico      
tt.transacao         = pTransacao  .

























        
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
  DISPLAY fiDtIni fiDtFim tg_compensacao_cr tg_compensacao_ap 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fiDtIni fiDtFim tg_compensacao_cr tg_compensacao_ap 
         btExecutar tglog 
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

  {utp/ut9000.i "esfin999" "12.1.07.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  IF c-seg-usuario <> 'super' THEN
     ASSIGN tglog:VISIBLE IN FRAME {&FRAME-NAME} = NO .
  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornarCtaPortador w-livre 
PROCEDURE retornarCtaPortador :
/*------------------------------------------------------------------------------
  Retorna a conta cont†bil conforme o Portador e finalidade economica passada
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcodPortador AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcodFinalidEcon AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cContaContabil  AS CHARACTER   NO-UNDO.
FIND FIRST portad_finalid_econ NO-LOCK
    WHERE portad_finalid_econ.cod_portador     = pCodPortador
    AND   portad_finalid_econ.cod_finalid_econ = pCodFinalidEcon
    NO-ERROR.
IF AVAIL portad_finalid_econ THEN DO:
    FIND FIRST cta_corren_cta_ctbl
        WHERE cta_corren_cta_ctbl.cod_cta_corren = portad_finalid_econ.cod_cta_corren
        AND  cta_corren_cta_ctbl.dat_inic_valid <= TODAY
        AND  cta_corren_cta_ctbl.dat_fim_valid >= TODAY
        NO-LOCK NO-ERROR.
    IF AVAIL cta_corren_cta_ctbl THEN DO:
       ASSIGN cContaContabil = cta_corren_cta_ctbl.cod_cta_ctbl.
    END.
END.





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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

