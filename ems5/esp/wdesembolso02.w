&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

/*tabela temporaria dos valores a serem exibidos na planilha */
DEFINE TEMP-TABLE tt
    FIELD origem            AS CHAR FORMAT 'x(100)'
    FIELD cod_empresa       AS CHAR
    FIELD cod_estab         AS CHAR
    FIELD cod_emitente      AS CHAR FORMAT 'x(50)'
    FIELD desc_emitente     AS CHAR FORMAT 'x(200)'
    FIELD data              AS DATE
    /*FIELD cod_classif       AS INT
    FIELD desc_classif      AS CHAR FORMAT 'x(50)'*/
    FIELD conta_contabil    AS CHAR FORMAT 'x(12)'
    FIELD valor             AS DECIMAL  
    FIELD conta_corrente    AS CHAR FORMAT 'x(20)'
    FIELD base              AS CHAR 
    FIELD DESC_conta        AS CHAR FORMAT 'x(150)'
    FIELD tipo              AS CHAR
    FIELD cc                AS CHAR 
    FIELD DESC_cc           AS CHAR FORMAT 'x(50)'
    FIELD rowidNota         AS ROWID
    FIELD id_movto_corren   AS INT
    FIELD sequencia         AS INT.

/* tabela temporaria que armazena as contas, os centros de custo por sequencia da nota fiscal de origem do titulo a pagar*/
DEFINE TEMP-TABLE ttApropItem
       FIELD rRowidnota     AS ROWID
       FIELD conta          AS CHAR FORMAT 'x(12)'
       FIELD cc             AS CHAR        
       FIELD itCodigo       LIKE movto-estoq.it-codigo
       FIELD codRefer       LIKE movto-estoq.cod-refer
       FIELD seq            LIKE movto-estoq.sequen-nf
       FIELD valorMat       AS DECIMAL
       FIELD valorNf        AS DECIMAL.

 /*tabela temporaria que armazena a propor‡Æo de valor por conta e centro de custo a ser apropriado no titulo pago*/   
 DEFINE TEMP-TABLE ttApropRateio
      FIELD rowidnota   AS ROWID
      FIELD conta       AS CHAR FORMAT 'x(12)'
      FIELD cc          AS CHAR
      FIELD valor       AS DECIMAL
      FIELD perc        AS DECIMAL
      FIELD documento   AS CHAR
      FIELD serie       AS CHAR
      FIELD codEmitente AS INT
      FIELD natOperacao AS CHAR
      FIELD data        AS DATE .

DEFINE VARIABLE hLog AS HANDLE      NO-UNDO.

/*buffer do movto-tit-ap para buscar a apropria‡Æo da implanta‡Æo do titulo*/
DEFINE BUFFER bf_movto_tit_ap_01 FOR movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_02 FOR movto_tit_ap.
DEFINE BUFFER bf_movto_tit_ap_03 FOR movto_tit_ap.

DEFINE BUFFER bf_tit_ap_01  FOR tit_Ap.
DEFINE BUFFER bf_tit_ap_02  FOR tit_Ap.
DEFINE BUFFER bf_tit_ap_03  FOR tit_Ap.
DEFINE BUFFER bf_tit_ap_04  FOR tit_Ap.
DEFINE BUFFER bf_tit_ap_05  FOR tit_Ap.
/*DEFINE BUFFER bf_ttApropItem FOR ttApropItem .*/

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
&Scoped-Define ENABLED-OBJECTS rt-button fiDtIni fiDtFim btExecutar 
&Scoped-Define DISPLAYED-OBJECTS fiDtIni fiDtFim 

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
     LABEL "At‚" 
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiDtIni AT ROW 2.75 COL 15.57 COLON-ALIGNED WIDGET-ID 4
     fiDtFim AT ROW 2.79 COL 40.72 COLON-ALIGNED WIDGET-ID 6
     btExecutar AT ROW 2.79 COL 63.86 WIDGET-ID 2
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
         HEIGHT             = 3.71
         WIDTH              = 89.29
         MAX-HEIGHT         = 27.5
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 27.5
         VIRTUAL-WIDTH      = 194.86
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
  EMPTY TEMP-TABLE tt.
  EMPTY TEMP-TABLE ttApropRateio.
  EMPTY TEMP-TABLE ttApropItem.
  RUN esporadicos/LOG02.p PERSISTENT SET hLog.
  RUN arquivoSaida IN hLog('logDesembolso').
  /*RUN extensaoArquivo IN hlog('.html').       
  RUN incluirLog IN hLog('<html><body>','').    */

  ASSIGN btExecutar:LABEL = "Executando.."
         btExecutar:SENSITIVE = NO.
  RUN pibuscarDAdos.
  OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "desembolso.txt").
  FOR EACH tt
      WHERE tt.valor <> 0:

      EXPORT DELIMITER "|" tt EXCEPT tt.rowidnota.
  END.
  OUTPUT CLOSE.

 /* RUN incluirLog IN hLog('</body></html>','').*/
  RUN imprimirtxt IN hLog.
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
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
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
  DISPLAY fiDtIni fiDtFim 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fiDtIni fiDtFim btExecutar 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarApropFolha w-livre 
PROCEDURE piBuscarApropFolha :
/*------------------------------------------------------------------------------
  Quando o lan‡amento vem da folha ou ‚ imposto retido, pega a contrapartida do movimento de baixa diretamente
------------------------------------------------------------------------------*/

DEFINE VARIABLE valor AS DECIMAL     NO-UNDO.

DEFINE INPUT  PARAMETER rowidMovtoTitAP                 AS ROWID                                                NO-UNDO.
DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE movto_cta_corren.dat_transacao                     NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_03.val_movto_ap                          NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE movto_cta_corren.cod_cta_corren                    NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.

FIND bf_movto_tit_ap_03 
    WHERE ROWID(bf_movto_tit_ap_03) = RowidMovtoTitAp NO-LOCK NO-ERROR.
FIND FIRST bf_tit_ap_05 OF bf_movto_tit_ap_03 NO-LOCK NO-ERROR.
FIND FIRST emitente 
    WHERE cod-emitente = bf_tit_ap_05.cdn_fornecedor NO-LOCK NO-ERROR.

FOR EACH aprop_ctbl_ap OF bf_movto_tit_ap_03 NO-LOCK                                                                                                                   
    WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
      OR  (movto_cta_corren.ind_fluxo_movto_cta_corren  = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db')                                                  
     /*AND aprop_ctbl_ap.cod_indic_econ = 'real'*/ :
    RUN incluirLog IN hLog('agrupamento Geral','====inicio aprop_ctbl_ap - folha ou imposto========================================').
    RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor:' + string(aprop_ctbl_ap.val_aprop_ctbl)  + ' moeda:' + aprop_ctbl_ap.cod_indic_econ ).
    IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
        RUN incluirLog IN hLog('agrupamento Geral','nivel 2 - achou emitente?' + string(AVAIL emitente)).
        FIND FIRST val_aprop_ctbl_ap OF aprop_ctbl_ap
              WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
        IF AVAIL val_aprop_ctbl_ap THEN 
           ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl.
        ELSE
           ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
     END.
     ELSE DO:
         ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
     END.
     RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor convertido:' + string(valor)).
     CREATE tt.                                                                                                                                                   
     ASSIGN                                                                                                                                                       
     tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
     tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
     tt.cod_emitente      =  IF AVAIL bf_tit_ap_05 THEN string(bf_tit_ap_05.cdn_fornecedor) ELSE '0'                                                                          
     tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                   
     tt.data              =  pDataContaCorrente                                                                                                          
     tt.conta_contabil    =  aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
     tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
     tt.conta_corrente    = p_cod_cta_corren                                                                                                      
     tt.base              =  '10'                                                                                                                                 
     tt.origem            =  'Contas a Pagar - Folha/Imp.Retido'                                                                                                                     
     tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
     tt.cc                = aprop_ctbl_ap.cod_ccusto
     tt.sequencia         = p_num_seq_movto_cta_corren
     tt.id_movto_corren   = p_num_id_movto_cta_corren.
     RUN incluirLog IN hLog('agrupamento Geral','====fim aprop_ctbl_ap========================================').
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarApropNormal w-livre 
PROCEDURE piBuscarApropNormal :
/*------------------------------------------------------------------------------
  Quando o lan‡amento NÇO ‚ de folha pega a contrapartida de debito do movimento de implanta‡Æo do titulo
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE movto_cta_corren.dat_transacao                     NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE movto_tit_ap.val_movto_ap                          NO-UNDO.
DEFINE INPUT  PARAMETER p_val_juros                     LIKE movto_tit_ap.val_juros                             NO-UNDO.
DEFINE INPUT  PARAMETER p_val_desconto                  LIKE movto_tit_ap.val_desconto                          NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE movto_cta_corren.cod_cta_corren                    NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.
DEFINE INPUT  PARAMETER p_rowidTitAp                    AS ROWID                                                NO-UNDO.

DEFINE VARIABLE dIndice     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE valorInd    AS DECIMAL     NO-UNDO.



FIND  bf_tit_ap_01
    WHERE ROWID(bf_tit_ap_01) = p_rowidTitAp NO-LOCK NO-ERROR.
FIND FIRST emitente 
    WHERE cod-emitente = bf_tit_ap_01.cdn_fornecedor NO-LOCK NO-ERROR.

FIND FIRST bf_movto_tit_ap_01 OF bf_tit_ap_01                                                                                                         
    WHERE bf_movto_tit_ap_01.ind_trans_ap_abrev = 'IMPL'                                                                                        
    OR    bf_movto_tit_ap_01.ind_trans_ap_abrev  = 'SBND'  NO-LOCK NO-ERROR.                                                                    
                                                                                                                                             
/*verifica a diferenca de valor entra a baixa e a implantacao e cria indice proporcional                                                     
   para tratar valores retirados ou acrescidos ap¢s a implantacao do titulo*/        
ASSIGN  valorInd = p_val_movto_ap + p_val_juros - p_val_desconto.
IF bf_movto_tit_ap_01.val_movto_ap <> valorInd THEN DO:                 
   ASSIGN dIndice = bf_movto_tit_ap_01.val_movto_ap / valorInd.         
END.                                                                                                                                         
ELSE                                                                                                                                         
   ASSIGN dIndice = 1.  


FIND bf_tit_ap_02 OF bf_movto_tit_ap_01 NO-LOCK NO-ERROR.
IF  bf_movto_tit_ap_01.ind_trans_ap_abrev  = 'SBND' THEN DO:
    RUN piBuscarTitulosSubstituidos(rowid(bf_tit_ap_02),
                                    pDataContaCorrente,
                                    p_ind_fluxo_movto_cta_corren,                                       
                                    (p_val_movto_ap + p_val_juros - p_val_desconto), 
                                    p_cod_cta_corren,  
                                    p_num_seq_movto_cta_corren, 
                                    p_num_id_movto_cta_corren).
    NEXT.
END.


RUN incluirLog IN hLog('agrupamento Geral','====inicio apropNormal - achou bf_movto?' + string(AVAIL bf_movto_tit_ap_01) + ' p========================================').

FOR EACH aprop_ctbl_ap OF bf_movto_tit_ap_01 NO-LOCK                                                                                                                   
     WHERE  (p_ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
      OR  (p_ind_fluxo_movto_cta_corren   = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db') :
    RUN incluirLog IN hLog('agrupamento Geral','====inicio aprop_ctbl_ap========================================').
    RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor:' + string(aprop_ctbl_ap.val_aprop_ctbl)  + ' moeda:' + aprop_ctbl_ap.cod_indic_econ ).
    IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
        RUN incluirLog IN hLog('agrupamento Geral','nivel 2 - achou emitente?' + string(AVAIL emitente)).
        FIND FIRST val_aprop_ctbl_ap OF aprop_ctbl_ap
              WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
        IF AVAIL val_aprop_ctbl_ap THEN 
           ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl / dIndice.
        ELSE
           ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
     END.
     ELSE DO:
         ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
     END.
     RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - aprop_ctbl_ap - valor convertido:' + string(valor)).
     CREATE tt.                                                                                                                                                   
     ASSIGN                                                                                                                                                       
     tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
     tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
     tt.cod_emitente      =  IF AVAIL bf_tit_ap_01 THEN string(bf_tit_ap_01.cdn_fornecedor) ELSE '0'                                                                          
     tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                   
     tt.data              =  pDataContaCorrente                                                                                                         
     tt.conta_contabil    =  aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
     tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
     tt.conta_corrente    = p_cod_cta_corren                                                                                                      
     tt.base              =  '10'                                                                                                                                 
     tt.origem            =  'Contas a Pagar - Titulo'                                                                                                                     
     tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
     tt.cc                = aprop_ctbl_ap.cod_ccusto
     tt.sequencia         = p_num_seq_movto_cta_corren
     tt.id_movto_corren   = p_num_id_movto_cta_corren.
     RUN incluirLog IN hLog('agrupamento Geral','====fim aprop_ctbl_ap========================================').
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarDados w-livre 
PROCEDURE piBuscarDados :
DEFINE VARIABLE valor       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE rRowidNota  AS ROWID       NO-UNDO.


FOR EACH movto_cta_corren 
    WHERE movto_cta_corren.dat_transacao >= INPUT FRAME {&frame-name} fiDtIni
    AND   movto_cta_corren.dat_transacao <= INPUT FRAME {&frame-name} fiDtFim 
    AND   ind_tip_movto_cta_corren = 're' /*apenas movimentos realizados*/
    AND   movto_cta_corren.cod_tip_trans_cx <> '002' /*transferencias entre contas*/
    AND   movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai'  NO-LOCK:
    RUN incluirLog IN hLog('agrupamento Geral','inicio movto_cta_corren ========================= data:' + STRING(movto_cta_corren.dat_transacao) + "conta:" + movto_cta_corren.cod_cta_corren) .
    RUN incluirLog IN hLog('agrupamento Geral','nivel 1 - movto_cta_corren - valor:' +  string(val_movto_cta_corren) + ' sequencia: ' + STRING(movto_cta_corren.num_seq_movto_cta_corren)).
   
    FIND FIRST cta_corren OF movto_cta_corren
        WHERE cta_corren.cod_empresa = string(i-ep-codigo-usuario) + '00'
        NO-LOCK NO-ERROR.
    IF NOT AVAIL cta_corren THEN DO: 
       RUN incluirLog IN hLog('agrupamento Geral','fim movto_cta_corren=====EMPRESA DA CONTA CORRENTE DIFERENTE DA CORRENTE===================='). 
       NEXT.                                                                      
    END.
    CASE movto_cta_corren.cod_modul_dtsul:
        WHEN 'acr'  THEN DO:
           FOR EACH movto_tit_acr OF movto_cta_corren NO-LOCK:
               FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.                                                                                                               
               FIND FIRST emitente WHERE                                                                                                                                           
                    tit_acr.cdn_cliente =  emitente.cod-emitente NO-LOCK NO-ERROR. 
/*                MESSAGE AVAIL tit_acr                  */
/*                    VIEW-AS ALERT-BOX INFO BUTTONS OK. */
               FOR EACH aprop_ctbl_acr OF movto_tit_acr                                                                                                                               
                   WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr')                                                       
                    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db') NO-LOCK :        
/*                    MESSAGE 'entrei no acr para criar tt'  */
/*                        VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                   CREATE tt.                                                                                                                                                         
                   ASSIGN                                                                                                                                                             
                   tt.cod_empresa       = aprop_ctbl_acr.cod_empresa                                                                                                                  
                   tt.cod_estab         = aprop_ctbl_acr.cod_estab                                                                                                                    
                   tt.cod_emitente      =  IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'                                                                                
                   tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit ELSE ''                                                                                          
                   tt.data              =  movto_cta_corren.dat_transacao                                                                                                               
                   tt.conta_contabil    =  aprop_ctbl_acr.cod_cta_ctbl                                                                                                                
                   tt.valor             =  IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  aprop_ctbl_acr.val_aprop_ctbl  ELSE  aprop_ctbl_acr.val_aprop_ctbl * -1       
                   tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                            
                   tt.base              =  '10'                                                                                                                                       
                   tt.origem            = 'Contas a Receber'                                                                                                                          
                   tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                                 
                   tt.cc                = aprop_ctbl_acr.cod_ccusto
                   tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                   tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren.                                                                                                                  .
               END.
           END.
        END.
        WHEN 'apb'  THEN DO:
           FOR EACH movto_tit_ap OF movto_cta_corren
               NO-LOCK:
               RUN incluirLog IN hLog('agrupamento Geral','==inicio movto_tit_ap========================================').
               FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR. 
               FIND FIRST ems5.espec_docto OF tit_ap NO-LOCK NO-ERROR.
               RUN incluirLog IN hLog('agrupamento Geral','nivel 2-movto_tit_ap - valor:' +  string(movto_tit_ap.val_movto_ap) ).
               FIND FIRST emitente                                                                                                                                              
                   WHERE tit_ap.cdn_fornecedor = emitente.cod-emitente NO-LOCK NO-ERROR. 
               RUN incluirLog IN hLog('agrupamento Geral','nivel 2- achou titulo?' + string(AVAIL tit_ap)).
               RUN incluirLog IN hLog('agrupamento Geral','nivel 2- achou emitente?' + string(AVAIL emitente)).
               
               IF (AVAIL tit_ap AND tit_ap.ind_origin_tit_ap <> "REC" /*origem recebimento*/ ) OR NOT AVAIL tit_Ap  THEN DO:
                  RUN incluirLog IN hLog('agrupamento Geral','nivel 3- Origem NAO EH Recebimento ou Movimento Sem Titulo').
                  IF AVAIL tit_ap THEN DO:
                     IF SUBSTR(tit_ap.cod_tit_ap,1,2) = 'hr' OR espec_docto.ind_tip_espec_docto = 'imposto retido' THEN DO : /*caso o titulo seja da folha*/     
                        RUN piBuscarApropFolha( ROWID(movto_tit_ap),
                                                movto_cta_corren.dat_transacao,
                                                movto_cta_corren.ind_fluxo_movto_cta_corren,
                                                movto_tit_ap.val_movto_ap,
                                                movto_cta_corren.cod_cta_corren,
                                                movto_cta_corren.num_seq_movto_cta_corren,                 
                                                movto_cta_corren.num_id_movto_cta_corren ).
                     END.                                                                                  
                     ELSE DO:     
                        RUN incluirLog IN hLog('agrupamento Geral','====inicio apropNormal - achou titulo?' + string(AVAIL tit_ap) + ' p========================================').
                        
                        RUN piBuscarApropNormal(movto_cta_corren.dat_transacao,                            
                                                movto_cta_corren.ind_fluxo_movto_cta_corren,               
                                                movto_tit_ap.val_movto_ap,                                 
                                                movto_tit_ap.val_juros,                                    
                                                movto_tit_ap.val_desconto,                                 
                                                movto_cta_corren.cod_cta_corren,                           
                                                movto_cta_corren.num_seq_movto_cta_corren,                 
                                                movto_cta_corren.num_id_movto_cta_corren,                  
                                                ROWID(tit_ap)
                                                ).                
                     END.                                                                                  
                  END.
                  ELSE DO:
                     RUN incluirLog IN hLog('agrupamento Geral','nivel 3- Lancamento com ORIGEM no AP, PEF ou Adiantamento:' + string(movto_tit_ap.val_movto_ap)).
                     FOR EACH antecip_pef_pend OF movto_tit_ap  NO-LOCK:
                         FOR EACH aprop_ctbl_pend_ap OF antecip_pef_pend
                             NO-LOCK:
                             CREATE tt.                                                                                                                                                   
                             ASSIGN                                                                                                                                                       
                             tt.cod_empresa       = aprop_ctbl_pend_ap.cod_empresa                                                                                                             
                             tt.cod_estab         = aprop_ctbl_pend_ap.cod_estab                                                                                                               
                             tt.cod_emitente      =  IF AVAIL tit_ap THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                             tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                   
                             tt.data              =  movto_cta_corren.dat_transacao                                                                                                        
                             tt.conta_contabil    =  aprop_ctbl_pend_ap.cod_cta_ctbl                                                                                                           
                             tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  movto_tit_ap.val_movto_ap ELSE movto_tit_ap.val_movto_ap * -1     
                             tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                      
                             tt.base              =  '10'                                                                                                                                 
                             tt.origem            =  'Contas a Pagar - PEF'                                                                                                                     
                             tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                             tt.cc                = aprop_ctbl_pend_ap.cod_ccusto
                             tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                             tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren.

                         END.
                     END.
                  END.
               END.
               ELSE DO:
                   RUN incluirLog IN hLog('agrupamento Geral','nivel 3 - Origem Recebimento').
                   RUN piBuscarNotasRateio(tit_ap.cod_ser_docto,tit_ap.cod_tit_ap,tit_ap.cdn_fornecedor,tit_ap.cod_parcela, OUTPUT rRowidNota ).
                   FOR EACH ttApropRateio 
                       WHERE ttApropRateio.rowidNota = rRowidNota .
                       RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - ttApropRateio - Valor perc:' + string(ttApropRateio.perc) ).
                       CREATE tt.                                                                                                                                                   
                       ASSIGN                                                                                                                                                       
                       tt.cod_empresa       = IF AVAIL tit_ap THEN tit_ap.cod_empresa ELSE ''                                                                                                             
                       tt.cod_estab         = IF AVAIL tit_ap THEN tit_ap.cod_estab   ELSE ''                                                                                                             
                       tt.cod_emitente      = IF AVAIL tit_ap THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                       tt.desc_emitente     = IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                   
                       tt.data              = movto_cta_corren.dat_transacao                                                                                                          
                       tt.conta_contabil    = ttApropRateio.conta                                                                                                           
                       tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN movto_tit_ap.val_movto_ap * ttApropRateio.perc 
                                              ELSE  movto_tit_ap.val_movto_ap * -1 * ttApropRateio.perc     
                       tt.conta_corrente    = movto_cta_corren.cod_cta_corren                                                                                                      
                       tt.base              = '10'                                                                                                                                 
                       tt.origem            = 'Recebimento'                                                                                                                     
                       tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                       tt.cc                = ttApropRateio.cc
                       tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
                       tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren.
                   END.
               END.
               RUN incluirLog IN hLog('agrupamento Geral','==fim movto_tit_ap==================================').
           END.
        END.
        WHEN 'cmg'  THEN DO:
            FIND FIRST tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
            FOR EACH aprop_ctbl_cmg OF movto_cta_corren
                WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
                OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK :
               CREATE tt.
               ASSIGN 
               tt.cod_empresa       = aprop_ctbl_cmg.cod_empresa 
               tt.cod_estab         = aprop_ctbl_cmg.cod_estab
               tt.cod_emitente      = movto_cta_corren.cod_tip_trans_cx
               tt.desc_emitente     = IF AVAIL tip_trans_cx THEN tip_trans_cx.des_tip_trans_cx  ELSE ''
               tt.data              = movto_cta_corren.dat_transacao
               tt.conta_contabil    = aprop_ctbl_cmg.cod_cta_ctbl
               tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN aprop_ctbl_cmg.val_movto_cta_corren ELSE aprop_ctbl_cmg.val_movto_cta_corren * -1
               tt.conta_corrente    = movto_cta_corren.cod_cta_corren
               tt.base              = '10'
               tt.origem            = 'Caixa e Bancos'
               tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren
               tt.cc                = aprop_ctbl_cmg.cod_ccusto
               tt.sequencia         = movto_cta_corren.num_seq_movto_cta_corren
               tt.id_movto_corren   = movto_cta_corren.num_id_movto_cta_corren.    
            END.
        END.
    END CASE.
    RUN incluirLog IN hLog('agrupamento Geral','fim movto_cta_corren=========================').
END.
FOR EACH tt BREAK BY tt.conta_contabil:
    IF FIRST-OF(tt.conta_contabil) THEN DO:
       FIND FIRST cta_ctbl
           WHERE cta_ctbl.cod_cta_ctbl = tt.conta_contabil NO-LOCK NO-ERROR.
    END.
    ASSIGN tt.DESC_conta = cta_ctbl.DES_tit_ctbl.
END.
FOR EACH tt 
    WHERE tt.cc <> '' BREAK BY tt.cc:
    IF FIRST-OF(tt.cc) THEN DO:
       FIND FIRST ems5.ccusto
           WHERE ccusto.cod_ccusto = tt.cc NO-LOCK NO-ERROR.
    END.
    ASSIGN tt.DESC_cc = cCusto.Des_tit_ctbl.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarNotasRateio w-livre 
PROCEDURE piBuscarNotasRateio :
/*
------------------------------------------------------------------------------
 serie-docto -> cod_ser_docto
 nro-docto -> cod_tit_ap
 cod_emitente -> cdn_fornecedor
 nat_operacao -> n/d
 parcela -> cod_parcela
------------------------------------------------------------------------------
*/

DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDocumento   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEmitente AS INT         NO-UNDO.
DEFINE INPUT  PARAMETER pParcela     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pRowid       AS ROWID       NO-UNDO.
DEFINE VARIABLE dTotalBreak          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotalGeral          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cConta               AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
DEFINE VARIABLE cCC                  AS CHARACTER   NO-UNDO.
/*DEFINE OUTPUT PARAMETER pVlNota      AS DECIMAL     NO-UNDO.*/
/*DEFINE INPUT  PARAMETER pNatureza   AS CHARACTER   NO-UNDO.*/

/*EMPTY TEMP-TABLE ttApropItem.
EMPTY TEMP-TABLE ttApropRateio.*/
RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - Entrou na procedure piBuscarNotasRateio').

FIND FIRST dupli-apagar 
     WHERE dupli-aPagar.serie-docto     = pSerie
     AND   dupli-aPagar.nro-docto       = pDocumento
     AND   dupli-aPagar.cod-emitente    = pCodEmitente
     AND   dupli-aPagar.parcela         = pParcela
    NO-LOCK NO-ERROR.
RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou duplicata ?' + string(AVAIL dupli-apagar) ).
IF AVAIL dupli-apagar THEN DO:
   FIND FIRST docum-est OF dupli-apagar NO-LOCK NO-ERROR.
   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou nota de entrada ?' + string(AVAIL docum-est)).
   
   IF AVAIL docum-est THEN
      ASSIGN pRowid = ROWID(docum-est).
   ELSE
      ASSIGN pRowid = ? .
   FIND FIRST ttApropItem
       WHERE ttApropItem.rRowidNota = pRowid NO-LOCK NO-ERROR.
   RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - achou ttApropItem(se nÆo quer dizer que vai buscar o movimento de estoque) ?' + string(AVAIL ttAPropItem)).
   IF NOT AVAIL ttApropItem AND pRowid <> ? THEN DO:
      RUN incluirLog IN hLog('agrupamento Geral','nivel 4 - piBuscarNotasRateio - ACHOU ttApropItem e pRowid <> ? ').
      
      FOR EACH movto-estoq OF docum-est NO-LOCK.
          RUN incluirLog IN hLog('agrupamento Geral','======inicio movto-estoq========================================'). 
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - movto-estoq - valor' +  string(movto-estoq.valor-nota)).
          
          /* MESSAGE 'entrei no movimento de estoque - valor:' movto-estoq.valor-nota  
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

          FIND FIRST ITEM OF movto-estoq NO-LOCK NO-ERROR.
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - movto-estoq - cria‡Æo do ttapropitem - achou o item ? ' + STRING(AVAIL ITEM)).
          IF ITEM.tipo-contr = 2 /*total*/ THEN DO:
             IF movto-estoq.tipo-trans <> 1 THEN DO:
                RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== EH ITEM DE CONTROLE TOTAL E NAO EH ENTRADA ==============================').
                NEXT.
             END.
             ASSIGN cConta = movto-estoq.ct-saldo
                    cCC    = movto-estoq.sc-saldo.
          END.
          ELSE DO:
            IF ITEM.tipo-contr <> 2  THEN DO: 
               IF movto-estoq.tipo-trans = 1 THEN DO:
                  RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq====== NAO EH ITEM DE CONTROLE TOTAL E  EH ENTRADA ==============================').
                  NEXT.
               END.
               ASSIGN cConta = movto-estoq.ct-codigo
                      cCC    = movto-estoq.sc-codigo.
            END.
          END.

          /*FIND FIRST movto-estoq OF item-doc-est NO-LOCK NO-ERROR.*/                   
          /*FIND FIRST ttApropItem                                                         
              WHERE ttApropItem.rRowidnota = ROWID(docum-est)                            
              AND   ttApropItem.conta      =  movto-estoq.ct-codigo                      
              AND   ttApropItem.cc         =  movto-estoq.sc-codigo                      
              AND   ttApropItem.itCodigo   =  movto-estoq.it-codigo                      
              AND   ttApropItem.codRefer   =  movto-estoq.cod-refer                      
              AND   ttApropItem.seq        =  movto-estoq.sequen-nf                      
              NO-LOCK NO-ERROR.                                                      
          IF NOT AVAIL ttApropItem THEN DO: */     
          
          CREATE ttApropItem.                                                         
          ASSIGN ttApropItem.rRowidNota = ROWID(docum-est)                            
                 ttApropItem.conta      = cConta                   
                 ttApropItem.cc         = cCC
                 ttApropItem.itCodigo   = movto-estoq.it-codigo                       
                 ttApropItem.codRefer   = movto-estoq.cod-refer                       
                 ttApropItem.seq        = movto-estoq.sequen-nf                       
                 ttApropItem.valorMat   = movto-estoq.valor-mat-m[1]                  
                 ttApropItem.valorNf    = movto-estoq.valor-nota.                     
          /*END.*/
          RUN incluirLog IN hLog('agrupamento Geral','======fim movto-estoq========================================').
      END. 
   END.
   FIND FIRST ttApropRateio
       WHERE ttApropRateio.rowidNota = rRowidNota NO-LOCK NO-ERROR.
   IF NOT AVAIL ttApropRateio AND pRowid <> ? THEN DO: 
      /*calcula total para futura utilizacao no percentual*/
      FOR EACH ttApropItem
          WHERE ttAPropItem.rRowidNota = pRowid:
          ASSIGN dTotalGeral = dTotalGeral + ttApropItem.valorNf.
      END.
      
      FOR EACH ttApropItem
          WHERE ttAPropItem.rRowidNota = pRowid BREAK BY ttApropItem.conta BY ttApropItem.cc:
          RUN incluirLog IN hLog('agrupamento Geral','====== inicio cria‡Æo ttApropRateio========================================').
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor:' +  string(ttApropItem.valorNf) ).
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - tot-valor:' +  string(docum-est.tot-valor)  ).
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - icms complem:' +  string(docum-est.icm-complem)).
          ASSIGN dTotalBreak = dTotalBreak + ttAPropItem.valorNf.
          RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor dTotalBreak:' + string(dTotalBreak)).
          IF LAST-OF(ttApropItem.conta) AND LAST-OF(ttApropItem.cc) THEN DO:
             RUN incluirLog IN hLog('agrupamento Geral','nivel 5 - piBuscarNotasRateio - ttApropItem - valor dTotalBreak - LAST-OF - criacao ttApropRateio:' + string(dTotalBreak)).

             CREATE ttApropRateio.
             ASSIGN ttApropRateio.rowidNota   = rRowidNota
                    ttApropRateio.conta       = ttApropItem.conta
                    ttApropRateio.cc          = ttAPropItem.cc
                    ttApropRateio.valor       = dTotalBreak
                    ttApropRateio.perc        = dTotalBreak / dTotalGeral /*(docum-est.tot-valor + docum-est.icm-complem)*/
                    dTotalBreak               = 0
                    ttApropRateio.documento   = docum-est.nro-docto 
                    ttApropRateio.serie       = docum-est.serie-docto
                    ttApropRateio.codEmitente = docum-est.cod-emitente
                    ttApropRateio.natOperacao = docum-est.nat-Operacao                                                       
                    ttApropRateio.data        = docum-est.dt-trans .
          END.
          RUN incluirLog IN hLog('agrupamento Geral','====== fim cria‡Æo ttApropRateio========================================').
      END.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarTitulosSubstituidos w-livre 
PROCEDURE piBuscarTitulosSubstituidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER rowidTitAP                      AS ROWID                                                NO-UNDO.
DEFINE INPUT  PARAMETER pDataContaCorrente              LIKE movto_cta_corren.dat_transacao                     NO-UNDO.
DEFINE INPUT  PARAMETER p_ind_fluxo_movto_cta_corren    LIKE movto_cta_corren.ind_fluxo_movto_cta_corren        NO-UNDO.
DEFINE INPUT  PARAMETER p_val_movto_ap                  LIKE bf_movto_tit_ap_02.val_movto_ap                    NO-UNDO.
DEFINE INPUT  PARAMETER p_cod_cta_corren                LIKE movto_cta_corren.cod_cta_corren                    NO-UNDO.
DEFINE INPUT  PARAMETER p_num_seq_movto_cta_corren      LIKE movto_cta_corren.num_seq_movto_cta_corren          NO-UNDO.
DEFINE INPUT  PARAMETER p_num_id_movto_cta_corren       LIKE movto_cta_corren.num_id_movto_cta_corren           NO-UNDO.

DEFINE VARIABLE rRowidNota AS ROWID  NO-UNDO.

FIND bf_tit_ap_03
    WHERE ROWID(bf_tit_ap_03) = rowidTitAp NO-LOCK NO-ERROR.

/*
1 - busca os titulos que geraram a duplicata, partindo da premissa que o titulo corrente ‚ uma duplicata.
2 - chama as procedures para de busca de informa‡äes conforme a origem do titulo encontrado.

*/
FOR EACH bf_movto_tit_ap_02 NO-LOCK 
        WHERE ind_trans_ap_abrev        = 'BXSB'
        AND   bf_movto_tit_ap_02.num_fatur_ap = bf_tit_ap_03.num_fatur_ap.
    FIND FIRST bf_tit_ap_04 OF bf_movto_tit_ap_02 NO-LOCK NO-ERROR.
    FIND FIRST emitente                                                                                                                                              
         WHERE bf_tit_ap_04.cdn_fornecedor = emitente.cod-emitente NO-LOCK NO-ERROR. 

    IF bf_tit_ap_04.ind_origin_tit_ap <> "REC" /*origem recebimento*/  THEN DO:
       RUN incluirLog IN hLog('agrupamento Geral','nivel 6- Tit- Substituido - Origem NAO EH Recebimento').
       IF SUBSTR(bf_tit_ap_04.cod_tit_ap,1,2) = 'hr' THEN DO : /*caso o titulo seja da folha*/   
          RUN incluirLog IN hLog('agrupamento Geral','nivel 6- Tit- Substituido - Origem NAO EH Recebimento - HR').
          RUN piBuscarApropFolha( ROWID(bf_movto_tit_ap_02),
                                  pDataContaCorrente,
                                  p_ind_fluxo_movto_cta_corren,
                                  bf_movto_tit_ap_02.val_movto_ap,
                                  p_cod_cta_corren,
                                  p_num_seq_movto_cta_corren,                 
                                  p_num_id_movto_cta_corren ).
       END.
       ELSE
          RUN piBuscarApropNormal(pDataContaCorrente,                 
                                  p_ind_fluxo_movto_cta_corren,    
                                  bf_movto_tit_ap_02.val_movto_ap,                      
                                  bf_movto_tit_ap_02.val_juros,                         
                                  bf_movto_tit_ap_02.val_desconto,                      
                                  p_cod_cta_corren,                
                                  p_num_seq_movto_cta_corren,      
                                  p_num_id_movto_cta_corren,       
                                  ROWID(bf_tit_ap_04)).             


    END.
    ELSE DO:
           RUN incluirLog IN hLog('agrupamento Geral','nivel 6- Tit- Substituido - Origem Recebimento').
           RUN piBuscarNotasRateio(bf_tit_ap_04.cod_ser_docto,
                                   bf_tit_ap_04.cod_tit_ap,
                                   bf_tit_ap_04.cdn_fornecedor,
                                   bf_tit_ap_04.cod_parcela, 
                                   OUTPUT rRowidNota ).
           FOR EACH ttApropRateio 
               WHERE ttApropRateio.rowidNota = rRowidNota .
               RUN incluirLog IN hLog('agrupamento Geral','nivel 6 - titulo substituido - ttApropRateio - Valor perc:' + string(ttApropRateio.perc) ).
               CREATE tt.                                                                                                                                                   
               ASSIGN                                                                                                                                                       
               tt.cod_empresa       = IF AVAIL bf_tit_ap_04 THEN bf_tit_ap_04.cod_empresa ELSE ''                                                                                                             
               tt.cod_estab         = IF AVAIL bf_tit_ap_04 THEN bf_tit_ap_04.cod_estab   ELSE ''                                                                                                             
               tt.cod_emitente      = IF AVAIL bf_tit_ap_04 THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
               tt.desc_emitente     = IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                   
               tt.data              = pDataContaCorrente                                                                                                          
               tt.conta_contabil    = ttApropRateio.conta                                                                                                           
               tt.valor             = IF p_ind_fluxo_movto_cta_corren = 'ent' THEN bf_movto_tit_ap_02.val_movto_ap * ttApropRateio.perc 
                                      ELSE  bf_movto_tit_ap_02.val_movto_ap * -1 * ttApropRateio.perc     
               tt.conta_corrente    = p_cod_cta_corren                                                                                                      
               tt.base              = '10'                                                                                                                                 
               tt.origem            = 'Recebimento'                                                                                                                     
               tt.tipo              = p_ind_fluxo_movto_cta_corren                                                                                           
               tt.cc                = ttApropRateio.cc
               tt.sequencia         = p_num_seq_movto_cta_corren
               tt.id_movto_corren   = p_num_id_movto_cta_corren.
          END.
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

