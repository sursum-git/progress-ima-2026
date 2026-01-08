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

DEFINE TEMP-TABLE tt
    FIELD cod_empresa               AS CHAR
    FIELD data_transacao            AS DATE
    FIELD valor                     AS DECIMAL
    FIELD cod_modulo                AS CHAR
    FIELD num_documento             AS CHAR FORMAT "x(100)"
    FIELD cod_emitente              AS INT
    FIELD cod_serie                 AS CHAR FORMAT "x(100)"
    FIELD cod_parcela               AS CHAR FORMAT "x(100)"
    FIELD cod_especie               AS CHAR FORMAT "x(100)"
    FIELD cod_cta_ctbl              AS CHAR FORMAT "X(20)"
    FIELD descricao_conta           AS CHAR FORMAT "X(100)"
    FIELD ind_natur_lancto_ctbl     AS  CHAR 
    FIELD ano                       AS INT
    FIELD mes                       AS INT
    FIELD usuario                   AS CHAR 
    FIELD contabiliza               AS LOG
    FIELD contabilizado             AS LOG
    FIELD estornado                 AS LOG
    FIELD correcao                  AS DECIMAL
    FIELD mutuo                     AS LOGICAL
    FIELD cod_tip_trans_cx          AS CHAR FORMAT 'x(150)'
    FIELD valor_caixa               AS DECIMAL .
DEFINE TEMP-TABLE ttContas
    FIELD cod_conta AS CHAR FORMAT 'x(20)'.

DEFINE VARIABLE deValor AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cListaFinalidade AS CHARACTER  FORMAT 'x(20)' NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rt-button fiDtInicial fiDtFinal ~
cb_finalidade tg_cb tg_cr tg_ap btBuscar tg_mutuointer 
&Scoped-Define DISPLAYED-OBJECTS fiDtInicial fiDtFinal cb_finalidade tg_cb ~
tg_cr tg_ap tg_mutuointer 

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
DEFINE BUTTON btBuscar 
     LABEL "Gerar Excel" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cb_finalidade AS CHARACTER FORMAT "X(25)":U 
     LABEL "Finalidade" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fiDtFinal AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDtInicial AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tg_ap AS LOGICAL INITIAL yes 
     LABEL "Contas a Pagar" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg_cb AS LOGICAL INITIAL yes 
     LABEL "Caixa e Bancos" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .83 NO-UNDO.

DEFINE VARIABLE tg_cr AS LOGICAL INITIAL yes 
     LABEL "Contas a Receber" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg_mutuointer AS LOGICAL INITIAL yes 
     LABEL "Apenas Mutuo MED/IMA" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.86 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiDtInicial AT ROW 3 COL 13.57 COLON-ALIGNED WIDGET-ID 10
     fiDtFinal AT ROW 3 COL 61 COLON-ALIGNED WIDGET-ID 12
     cb_finalidade AT ROW 4.5 COL 13.72 COLON-ALIGNED WIDGET-ID 16
     tg_cb AT ROW 4.58 COL 63 WIDGET-ID 18
     tg_cr AT ROW 5.46 COL 63 WIDGET-ID 22
     tg_ap AT ROW 6.33 COL 63.14 WIDGET-ID 20
     btBuscar AT ROW 6.42 COL 15 WIDGET-ID 6
     tg_mutuointer AT ROW 7.25 COL 63.14 WIDGET-ID 24
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
         HEIGHT             = 8.38
         WIDTH              = 90
         MAX-HEIGHT         = 27.5
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.5
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


&Scoped-define SELF-NAME btBuscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBuscar w-livre
ON CHOOSE OF btBuscar IN FRAME f-cad /* Gerar Excel */
DO:
  
 IF fiDtInicial:SCREEN-VALUE > fiDtFinal:SCREEN-VALUE THEN DO:
    MESSAGE 'A data Inicial deve ser menor que a data Final'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
 END.

 EMPTY TEMP-TABLE tt.
 EMPTY TEMP-TABLE ttContas.
 ASSIGN btBuscar:LABEL     = "Processando ..."
         btBuscar:SENSITIVE = NO.

  /*busca Contas do processo do mutuo*/
  RUN buscarContasMutuo.

  OUTPUT TO c:\temp\ttcontas.txt.
  FOR EACH ttContas:
      DISP ttContas.
  END.
  OUTPUT CLOSE.

  
  
  
  
  /*busca movimentos do contas a pagar*/
  IF INPUT FRAME {&frame-name} tg_ap = YES THEN
     RUN buscarContasaPagar.
  

  
  /*busca movimentos do contas a receber*/
  IF INPUT FRAME {&frame-name} tg_cr = YES THEN
     RUN buscarContasaReceber.
  
  /*busca movimentos do caixa e bancos*/
  IF INPUT FRAME {&frame-name} tg_cb = YES THEN
     RUN buscarCaixaeBancos.  

  OUTPUT TO c:\temp\mutuo.txt.

  FOR EACH tt:
      FIND FIRST cta_ctbl
          WHERE cta_ctbl.cod_cta_ctbl = tt.cod_cta_ctbl NO-LOCK NO-ERROR.
      IF AVAIL cta_ctbl THEN
         ASSIGN tt.descricao_conta = cta_ctbl.des_tit_ctbl .
      EXPORT DELIMITER "|" tt.
  END.
  OUTPUT CLOSE.

  OS-COMMAND SILENT  VALUE(" start excel /n t:\especificos\excel\diarioauxiliar.xlsx").
  ASSIGN btBuscar:LABEL     = "Gerar Excel"
         btBuscar:SENSITIVE = YES.


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
       RUN set-position IN h_p-exihel ( 1.13 , 74.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiDtInicial:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarCaixaeBancos w-livre 
PROCEDURE buscarCaixaeBancos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH movto_cta_corren
      WHERE movto_cta_corren.dat_transacao >= INPUT FRAME {&frame-name} fiDtInicial
      AND   movto_cta_corren.dat_transacao <= INPUT FRAME {&frame-name} fiDtFinal
      NO-LOCK.

      IF movto_cta_corren.cod_tip_trans_cx = '14'  AND INPUT FRAME {&FRAME-NAME} tg_mutuointer   = YES THEN NEXT.

      
      FIND FIRST cta_corren OF movto_cta_corren NO-LOCK NO-ERROR.
      FIND FIRST tip_trans_cx
          WHERE tip_trans_cx.cod_tip_trans_cx = movto_cta_corren.cod_tip_trans_cx
          NO-LOCK NO-ERROR.
      FOR EACH  aprop_ctbl_cmg OF movto_cta_corren:
          
          /*WHERE LOOKUP(aprop_ctbl_cmg.cod_cta_ctbl,edContas:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 0 
          NO-LOCK NO-ERROR.
      
      IF AVAIL aprop_ctbl_cmg THEN DO:                                                                 */
         CREATE tt.
         FIND FIRST ttContas WHERE
         ttContas.cod_conta =  aprop_ctbl_cmg.cod_cta_ctbl NO-LOCK NO-ERROR.
         IF AVAIL ttContas THEN
            ASSIGN tt.mutuo = YES.
         ELSE
            ASSIGN tt.mutuo = NO.
         ASSIGN tt.cod_empresa              = aprop_ctbl_cmg.cod_empresa
                tt.data_transacao           = movto_cta_corren.dat_transacao
                tt.valor                    = IF aprop_ctbl_cmg.ind_natur_lancto_ctbl = "CR" THEN aprop_ctbl_cmg.val_movto_cta_corren * -1 ELSE aprop_ctbl_cmg.val_movto_cta_corren
                tt.cod_modulo               = "CMG"
                tt.num_documento            = cod_docto_movto_cta_bco /* +  "-" + des_histor_movto_cta_corren*/
                tt.cod_emitente             = 0
                tt.cod_serie                = cod_modul_dtsul
                tt.cod_parcela              = cta_corren.cod_agenc_bcia
                tt.cod_especie              = movto_cta_corren.cod_cta_corren
                tt.cod_cta_ctbl             = aprop_ctbl_cmg.cod_cta_ctbl
                tt.ind_natur_lancto_ctbl    = aprop_ctbl_cmg.ind_natur_lancto_ctbl
                tt.ano                      = YEAR(movto_cta_corren.dat_transacao)
                tt.mes                      = MONTH(movto_cta_corren.dat_transacao)
                tt.usuario                  = movto_cta_corren.cod_usuar_ult_atualiz
                tt.contabiliza              = YES       
                tt.contabilizado            = movto_cta_corren.log_ctbz_movto_cta_corren        
                tt.estornado                = NO
                tt.cod_tip_trans_cx         = IF AVAIL tip_trans_cx THEN des_tip_trans_cx ELSE ''
                tt.valor_caixa              = IF ind_fluxo_movto_cta_corren = 'ent' THEN tt.valor ELSE tt.valor * - 1 .
      END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarContasaPagar w-livre 
PROCEDURE buscarContasaPagar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH movto_tit_ap
      WHERE movto_tit_ap.dat_transacao >= INPUT FRAME {&frame-name} fiDtInicial
      AND movto_tit_ap.dat_transacao   <= INPUT FRAME {&frame-name} fiDtFinal
      AND movto_tit_ap.log_ctbz_aprop_ctbl = YES NO-LOCK.
      FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.
      
      ASSIGN deValor = 0.
      FOR EACH aprop_ctbl_ap OF movto_tit_ap NO-LOCK.
          
          FOR EACH val_aprop_ctbl_ap OF aprop_ctbl_ap NO-LOCK
              WHERE cod_finalid_econ = cb_finalidade:SCREEN-VALUE :

              CREATE tt.                                                                                                                                                                             
              FIND FIRST ttContas WHERE
              ttContas.cod_conta =  aprop_ctbl_ap.cod_cta_ctbl NO-LOCK NO-ERROR.
             IF AVAIL ttContas THEN
                ASSIGN tt.mutuo = YES.
             ELSE
              ASSIGN tt.mutuo = NO.
              ASSIGN tt.cod_empresa              = movto_tit_ap.cod_empresa                                                                                                                          
                     tt.data_transacao           = aprop_ctbl_ap.dat_transacao                                                                                                                       
                     tt.valor                    = IF aprop_ctbl_ap.ind_natur_lancto_ctbl = "CR" THEN val_aprop_ctbl_ap.val_aprop_ctbl * -1 ELSE val_aprop_ctbl_ap.val_aprop_ctbl                            
                     tt.cod_modulo               = "APB"                                                                                                                                             
                     tt.num_documento            = IF AVAIL tit_ap THEN tit_ap.cod_tit_ap ELSE ""                                                                                                    
                     tt.cod_emitente             = movto_tit_ap.cdn_fornecedor                                                                                                                       
                     tt.cod_serie                = IF AVAIL tit_ap THEN tit_ap.cod_ser_docto ELSE ""                                                                                                 
                     tt.cod_parcela              = IF AVAIL tit_ap THEN tit_ap.cod_parcela ELSE ""                                                                                                   
                     tt.cod_especie              = IF AVAIL tit_ap THEN tit_ap.cod_espec_docto ELSE ""                                                                                               
                     tt.cod_cta_ctbl             = aprop_ctbl_ap.cod_cta_ctbl                                                                                                                        
                     tt.ind_natur_lancto_ctbl    = aprop_ctbl_ap.ind_natur_lancto_ctbl                                                                                                               
                     tt.ano                      = YEAR(movto_tit_ap.dat_transacao)                                                                                                                  
                     tt.mes                      = MONTH(movto_tit_ap.dat_transacao)                                                                                                                 
                     tt.usuario                  = movto_tit_ap.cod_usuario                                                                                                                          
                     tt.contabiliza              = movto_tit_ap.log_ctbz_aprop_ctbl                                                                                                                  
                     tt.contabilizado            = movto_tit_ap.log_aprop_ctbl_ctbzda                                                                                                                
                     tt.estornado                = movto_tit_ap.log_movto_estordo                                                                                                                    
                     tt.correcao                 = movto_tit_ap.val_cm_tit_ap.  
          END.
      END.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarContasaReceber w-livre 
PROCEDURE buscarContasaReceber :
FOR EACH movto_tit_acr
      WHERE movto_tit_acr.dat_transacao >= INPUT FRAME {&frame-name} fiDtInicial
      AND   movto_tit_acr.dat_transacao <= INPUT FRAME {&frame-name} fiDtFinal
      NO-LOCK.
      FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.
      FOR EACH aprop_ctbl_acr OF movto_tit_acr NO-LOCK:
      
          /*WHERE LOOKUP(aprop_ctbl_acr.cod_cta_ctbl,edContas:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 0 NO-LOCK NO-ERROR.
      IF AVAIL aprop_ctbl_acr THEN DO:*/
         CREATE tt.
         FIND FIRST ttContas WHERE
           ttContas.cod_conta =  aprop_ctbl_acr.cod_cta_ctbl NO-LOCK NO-ERROR.
          IF AVAIL ttContas THEN
             ASSIGN tt.mutuo = YES.
          ELSE
             ASSIGN tt.mutuo = NO.
         ASSIGN tt.cod_empresa              = movto_tit_acr.cod_empresa
                tt.data_transacao           = movto_tit_acr.dat_transacao
                tt.valor                    = IF aprop_ctbl_acr.ind_natur_lancto_ctbl = "CR" THEN aprop_ctbl_acr.val_aprop_ctbl * -1 ELSE aprop_ctbl_acr.val_aprop_ctbl
                tt.cod_modulo               = "ACR"
                tt.num_documento            = IF AVAIL tit_acr THEN tit_acr.cod_tit_acr ELSE ""
                tt.cod_emitente             = movto_tit_acr.cdn_cliente
                tt.cod_serie                = IF AVAIL tit_acr THEN  tit_acr.cod_ser_docto ELSE ""
                tt.cod_parcela              = IF AVAIL tit_acr THEN tit_acr.cod_parcela ELSE ""
                tt.cod_especie              = IF AVAIL tit_acr THEN tit_acr.cod_espec_docto ELSE ""
                tt.cod_cta_ctbl             = aprop_ctbl_acr.cod_cta_ctbl
                tt.ind_natur_lancto_ctbl    = aprop_ctbl_acr.ind_natur_lancto_ctbl
                tt.ano                      = YEAR(movto_tit_acr.dat_transacao)
                tt.mes                      = MONTH(movto_tit_acr.dat_transacao)
                tt.usuario                  = movto_tit_acr.cod_usuario
                tt.contabiliza              = movto_tit_acr.log_ctbz_aprop_ctbl           
                tt.contabilizado            = movto_tit_acr.log_aprop_ctbl_ctbzda         
                tt.estornado                = movto_tit_acr.log_movto_estordo .       

      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarContasMutuo w-livre 
PROCEDURE buscarContasMutuo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
INPUT FROM t:\especificos\excel\mutuo.txt.
REPEAT:
  CREATE ttContas.
  IMPORT ttContas.

END.

INPUT CLOSE.


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
  DISPLAY fiDtInicial fiDtFinal cb_finalidade tg_cb tg_cr tg_ap tg_mutuointer 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fiDtInicial fiDtFinal cb_finalidade tg_cb tg_cr tg_ap 
         btBuscar tg_mutuointer 
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

  {utp/ut9000.i "esp995" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  FOR EACH finalid_econ NO-LOCK:
      ASSIGN  cListaFinalidade = IF cListaFinalidade = '' THEN finalid_econ.cod_finalid_econ + "," + finalid_econ.cod_finalid_econ
                                 ELSE cListaFinalidade + "," + finalid_econ.cod_finalid_econ + "," + finalid_econ.cod_finalid_econ.
  END.
  ASSIGN cb_finalidade:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}  = cListaFinalidade.
  ASSIGN cb_finalidade:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = 'CORRENTE'. 
  /*FIND FIRST im-param
      WHERE cod-param = 'contas-mutuo'
      NO-LOCK NO-ERROR.
  IF AVAIL im-param THEN DO:
     ASSIGN edContas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = im-param.val-param.
  END.*/
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

