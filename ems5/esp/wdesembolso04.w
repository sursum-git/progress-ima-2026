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
{include/i-prgvrs.i wdesembolso04 12.01.07.000}

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
         HEIGHT             = 3.21
         WIDTH              = 89.72
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90.29
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90.29
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
 ASSIGN btExecutar:LABEL = "Executando.."
        btExecutar:SENSITIVE = NO.
 ASSIGN arquivoSaida = 'desembolso.txt'.
 ASSIGN cAmbiente = ENTRY(44,SESSION:STARTUP-PARAMETERS,",").
 CASE cAmbiente:
     WHEN '8080' THEN
        ASSIGN digitoAmbiente      = '1'
               servidorAmbiente    = '192.168.0.44'
                dbems5             = '-db ems5 -H 192.168.0.4 -S 30032'
                cBaseAtual         = '10-Produ‡Æo'
                cBaseOutra         = '10-Backup'. 
     WHEN '8280' THEN
        ASSIGN digitoAmbiente      = '3'
               servidorAmbiente    = '192.168.0.4'
               dbems5              = '-db ems5 -H 192.168.0.44 -S 10032'
               cBaseAtual          = '12-Produ‡Æo'
               cBaseOutra          = '12-Backup' . 
     WHEN '8180' THEN
         ASSIGN digitoAmbiente     = '2'
               servidorAmbiente    = '192.168.0.44'
               dbems5              = '-db ems5 -H 192.168.0.4 -S 20032'
               cBaseAtual          = '10-Teste'
               cBaseOutra          = '10-Backup Teste' . 
     WHEN '8380' THEN
         ASSIGN digitoAmbiente     = '4'
               servidorAmbiente    = '192.168.0.4'
               dbems5              = '-db ems5 -H 192.168.0.44 -S 40032'
               cBaseAtual          = '12-Backup Teste'
               cBaseOutra          = '12-Teste' . 
     OTHERWISE  DO:
        MESSAGE 'Parametro de ambiente inconsistente, execu‡Æo abordada.Entre em contato com o setor de TI'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN btExecutar:LABEL = "Executar"
        btExecutar:SENSITIVE = YES.
        RETURN NO-APPLY.
     END.

 END CASE.
 OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + ArquivoSaida).
 /*limpar arquivo*/
 OUTPUT CLOSE.
  /* busca os dados da empresa corrente*/
  ASSIGN cEmpresaAtual = i-ep-codigo-usuario.
  RUN esp/desembolsorp.p
      (
        INPUT FRAME {&frame-name} fiDtIni ,
        INPUT FRAME {&frame-name} fiDtFim ,
        cBaseAtual,
        arquivoSaida,
        cEmpresaAtual
      ).                  

  /*conecta banco diferente da empresa atual*/
  CASE cBaseAtual:
      WHEN '5' THEN DO:
          IF CONNECTED('ems2med') THEN DO:
             DISCONNECT ems2med.
          END.    
          ASSIGN cConexao = '-db ems2ima -H ' + servidorambiente +  ' -S ' +  digitoAmbiente + '0030 '.
          RUN  deletarAliasEms2.
          RUN esporadicos\conectaBASE.p(cConexao).
          RUN criarAliasEms2('ems2ima').
          ASSIGN cEmpresaATual = '1'.
          RUN esp/desembolsorp.p
          (
          INPUT FRAME {&frame-name} fiDtIni ,
          INPUT FRAME {&frame-name} fiDtFim ,
          cBaseAtual,
          arquivoSaida,
          cEmpresaAtual
          ).

      END.
      WHEN '1' THEN DO:
          IF CONNECTED('ems2ima') THEN DO:
             DISCONNECT ems2ima.
          END.    
          ASSIGN cConexao = '-db ems2med -H ' + servidorambiente +  ' -S ' +  digitoAmbiente + '0031 '.
          RUN  deletarAliasEms2.
          RUN esporadicos\conectaBASE.p(cConexao).
          RUN criarAliasEms2('ems2med').
          ASSIGN cEmpresaAtual = '5'. 
          RUN esp/desembolsorp.p
          (
          INPUT FRAME {&frame-name} fiDtIni ,
          INPUT FRAME {&frame-name} fiDtFim ,
          cBaseAtual,
          arquivoSaida,
          cEmpresaAtual
          ).
      END.

  END CASE.
  
  /*** modifica o ems5 para base contraria a atual****/
 /* IF CONNECTED('ems5') THEN DO:
     DISCONNECT ems5.
/*      MESSAGE 'desconectei'                  */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  END.
  IF CONNECTED('ems5') THEN DO:
/*      MESSAGE 'nÆo desconectou'              */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  END.*/
  RUN deletarAliasEms5.
  ASSIGN cConexao = dbems5.
/*   MESSAGE 'vou conectar'                 */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  DISCONNECT ems5.
  CONNECT VALUE(cConexao).
/*   MESSAGE 'conectei'                     */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  RUN criarAliasEms5.
  ASSIGN cEmpresaAtual = '1'.
  RUN esp/desembolsorp.p
      (
        INPUT FRAME {&frame-name} fiDtIni ,
        INPUT FRAME {&frame-name} fiDtFim ,
        cBaseOutra,
        arquivoSaida,
        cEmpresaAtual
      ). 
  ASSIGN cEmpresaAtual = '5'.
  RUN esp/desembolsorp.p
      (
        INPUT FRAME {&frame-name} fiDtIni ,
        INPUT FRAME {&frame-name} fiDtFim ,
        cBaseOutra,
        arquivoSaida,
        cEmpresaAtual
      ).

  
  
  ASSIGN btExecutar:LABEL = "Executar"
          btExecutar:SENSITIVE = YES.
  /*IF CONNECTED('ems5') THEN DO:
     DISCONNECT ems5.
  END.*/
  IF CONNECTED('ems2ima') THEN DO:
     DISCONNECT ems2ima.
  END.
  IF CONNECTED('ems2med') THEN DO:
     DISCONNECT ems2ima.
  END.
  CASE cAmbiente:
      WHEN '8080' THEN DO:
          RUN deletarAliasEms5.
          ASSIGN cConexao = '-db ems5 -H 192.168.0.44 -S 10032'.
          DISCONNECT ems5.
          CONNECT VALUE(cConexao).
          RUN criarAliasEms5.
          RUN deletarAliasEms2.
          CASE cEmpresaAtual:
               WHEN '1'  THEN DO:

                   CONNECT VALUE('-db ems2ima -H 192.168.0.44 -S 10030') NO-ERROR.
                   RUN criarAliasEms2('ems2ima').
                   
               END.
               WHEN '5' THEN DO:
                   CONNECT VALUE('-db ems2med -H 192.168.0.44 -S 10031') NO-ERROR.
                   RUN criarAliasEms2('ems2med').
               END.
           END CASE.
           
      END.
      WHEN '8180' THEN DO:
          RUN deletarAliasEms5.
          ASSIGN cConexao = '-db ems5 -H 192.168.0.44 -S 20032'.
          DISCONNECT ems5.
          CONNECT VALUE(cConexao).
          RUN criarAliasEms5.
          RUN deletarAliasEms2.
          CASE cEmpresaAtual:
               WHEN '1'  THEN DO:
                   CONNECT VALUE('-db ems2ima -H 192.168.0.44 -S 20030') NO-ERROR.
                   RUN criarAliasEms2('ems2ima').
               END.
               WHEN '5' THEN DO:
                   CONNECT VALUE('-db ems2med -H 192.168.0.44 -S 20031') NO-ERROR.
                   RUN criarAliasEms2('ems2med').
               END.
           END CASE.
           
      END.
      WHEN '8280' THEN DO:
          RUN deletarAliasEms5.
          ASSIGN cConexao = '-db ems5 -H 192.168.0.4 -S 30032'.
          DISCONNECT ems5.
          CONNECT VALUE(cConexao).
          RUN criarAliasEms5.
          RUN deletarAliasEms2.
          CASE cEmpresaAtual:
               WHEN '1'  THEN DO:
                   CONNECT VALUE('-db ems2ima -H 192.168.0.4 -S 30030') NO-ERROR.
                   RUN criarAliasEms2('ems2ima').
               END.
               WHEN '5' THEN DO:
                   CONNECT VALUE('-db ems2med -H 192.168.0.4 -S 30031') NO-ERROR.
                   RUN criarAliasEms2('ems2med').
               END.
           END CASE.
           
      END.
      WHEN '8380' THEN DO:
          RUN deletarAliasEms5.
          ASSIGN cConexao = '-db ems5 -H 192.168.0.4 -S 40032'.
          DISCONNECT ems5.
          CONNECT VALUE(cConexao).
          RUN criarAliasEms5.
          RUN deletarAliasEms2.
          CASE cEmpresaAtual:
               WHEN '1'  THEN DO:
                   CONNECT VALUE('-db ems2ima -H 192.168.0.4 -S 40030') NO-ERROR.
                   RUN criarAliasEms2('ems2ima').
                   
               END.
               WHEN '5' THEN DO:
                   CONNECT VALUE('-db ems2med -H 192.168.0.4 -S 40031') NO-ERROR.
                   RUN criarAliasEms2('ems2med').
               END.
           END CASE.
      END.
  END CASE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criarAliasEms2 w-livre 
PROCEDURE criarAliasEms2 :
DEFINE INPUT  PARAMETER pBase AS CHARACTER   NO-UNDO.

CREATE ALIAS ems2ima FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS ems2med FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgadm   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgadm   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgdis   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgfis   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgcld   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgcex   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mginv   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgind   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgmfg   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgmnt   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgmrp   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgscm   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgdbr   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS emsdca  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS ems2oe  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mgsop   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS mguni   FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movadm  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movdis  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movfis  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movind  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movmfg  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movmnt  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS movdbr  FOR DATABASE value(pBase) NO-ERROR.
CREATE ALIAS wmovdis FOR DATABASE value(pBase) NO-ERROR.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criarAliasEms5 w-livre 
PROCEDURE criarAliasEms5 :
create alias emsbas     for database ems5       no-error.
create alias emsedi     for database ems5       no-error.
create alias emsfin     for database ems5       no-error.
create alias emsinc     for database ems5       no-error.
create alias emsnam     for database ems5       no-error.
create alias emsuni     for database ems5       no-error.
create alias emsven     for database ems5       no-error.
create alias movfin     for database ems5       no-error.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deletarAliasEms2 w-livre 
PROCEDURE deletarAliasEms2 :
DELETE ALIAS ems2ima .
DELETE ALIAS ems2med .
delete ALIAS mgadm   .
delete ALIAS mgdis   .
delete ALIAS mgfis   .
delete ALIAS mgcld   .
delete ALIAS mgcex   .
delete ALIAS mginv   .
delete ALIAS mgind   .
delete ALIAS mgmfg   .
delete ALIAS mgmnt   . 
delete ALIAS mgmrp   .
delete ALIAS mgscm   .
delete ALIAS mgdbr   .
delete ALIAS emsdca  .
delete ALIAS ems2oe  .
delete ALIAS mgsop   .
delete ALIAS mguni   .
delete ALIAS movadm  .
delete ALIAS movdis  .
delete ALIAS movfis  .
delete ALIAS movind  .
delete ALIAS movmfg  .
delete ALIAS movmnt  .
delete ALIAS movdbr  .
delete ALIAS wmovdis .





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deletarAliasEms5 w-livre 
PROCEDURE deletarAliasEms5 :
DELETE alias emsbas     .
DELETE alias emsedi     .
DELETE alias emsfin     .
DELETE alias emsinc     .
DELETE alias emsnam     .
DELETE alias emsuni     .
DELETE alias emsven     .
DELETE alias movfin     .

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

  {utp/ut9000.i "wdesembolso04" "12.1.07.000"}

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

