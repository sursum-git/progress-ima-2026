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

DEFINE VARIABLE hBoNat001 AS HANDLE      NO-UNDO.
DEFINE VAR NatOperacao            AS CHARACTER   NO-UNDO.
DEFINE VAR CodParamNatOperacao    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO FORMAT 'x(400)'.

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
&Scoped-Define ENABLED-OBJECTS rt-button RECT-2 RECT-1 fi_dt_ini fi_dt_fim ~
fi_cliente fi_pedido_ini fi_pedido_fim fi_cliente_triang fi_cod_estab ~
bt_Simular 
&Scoped-Define DISPLAYED-OBJECTS fi_dt_ini fi_dt_fim fi_cliente ~
fi_pedido_ini fi_pedido_fim fi_cliente_triang fi_cod_estab 

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
DEFINE BUTTON bt_Simular 
     LABEL "Simulaá∆o" 
     SIZE 15 BY 1.11.

DEFINE VARIABLE fi_cliente AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cliente_triang AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cliente Triang." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cod_estab AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_dt_fim AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Final" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79 NO-UNDO.

DEFINE VARIABLE fi_dt_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .79 NO-UNDO.

DEFINE VARIABLE fi_pedido_fim AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 999999 
     LABEL "Pedido Fim" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .79 NO-UNDO.

DEFINE VARIABLE fi_pedido_ini AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Pedido Inicial" 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 6.11.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.4 BY 6.05.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.47
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi_dt_ini AT ROW 3 COL 15 COLON-ALIGNED WIDGET-ID 24
     fi_dt_fim AT ROW 3 COL 34 COLON-ALIGNED WIDGET-ID 26
     fi_cliente AT ROW 3.63 COL 62 COLON-ALIGNED WIDGET-ID 34
     fi_pedido_ini AT ROW 4.26 COL 15 COLON-ALIGNED WIDGET-ID 30
     fi_pedido_fim AT ROW 4.26 COL 34 COLON-ALIGNED WIDGET-ID 28
     fi_cliente_triang AT ROW 4.95 COL 62 COLON-ALIGNED WIDGET-ID 38
     fi_cod_estab AT ROW 6.26 COL 62 COLON-ALIGNED WIDGET-ID 40
     bt_Simular AT ROW 8.9 COL 2.6 WIDGET-ID 12
     rt-button AT ROW 1 COL 1
     RECT-2 AT ROW 2.58 COL 49 WIDGET-ID 36
     RECT-1 AT ROW 2.53 COL 2 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.2 BY 14.05
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
         HEIGHT             = 9.32
         WIDTH              = 79.2
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90.6
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90.6
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


&Scoped-define SELF-NAME bt_Simular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_Simular w-livre
ON CHOOSE OF bt_Simular IN FRAME f-cad /* Simulaá∆o */
DO:
   ASSIGN bt_simular:SENSITIVE = NO.
   IF fi_cliente:SCREEN-VALUE <> "0" THEN
      RUN buscarCliente.
   ELSE
      RUN buscarPedidos.
   ASSIGN bt_simular:SENSITIVE = YES.
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
       RUN set-position IN h_p-exihel ( 1.11 , 63.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.26 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fi_dt_ini:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarCLiente w-livre 
PROCEDURE buscarCLiente :
/*------------------------------------------------------------------------------
  busca os pedidos, assim como seus itens, conforme os parametros de tela
------------------------------------------------------------------------------*/
DEFINE VARIABLE cNatOperacao            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCodParamNatOperacao    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFinalidade             AS CHAR        NO-UNDO.
DEFINE VARIABLE cTipoAtividade          AS CHAR        NO-UNDO FORMAT "x(8)".
DEFINE VARIABLE codCnae                 AS CHARACTER   NO-UNDO FORMAT "x(8)".
DEFINE VARIABLE cDescAtividade          AS CHARACTER   NO-UNDO FORMAT "x(100)".
DEFINE VARIABLE lTriangular             AS LOGICAL     NO-UNDO INIT NO.
/*MESSAGE 'pegadinha do malandro'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
MESSAGE 'quantas vezes ira repetir?'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
IF INPUT FRAME {&frame-name} fi_cliente_triang <> 0 THEN
   ASSIGN lTriangular = YES.
ELSE
   ASSIGN lTriangular = NO.

RUN esbo/bonat001.p PERSISTENT SET hBoNat001.
OUTPUT TO c:\temp\simulacao_nat_operacao_pedidos.txt.
FOR FIRST emitente NO-LOCK
    WHERE emitente.cod-emitente = INPUT FRAME {&frame-name} fi_cliente.
    
    /*
    MESSAGE 'vou chamar a natureza de operacao'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    RUN retornarFinalidadeCliente IN hBoNat001(emitente.cod-emitente, OUTPUT cFinalidade, OUTPUT  codCnae).
    RUN retornarTipoAtividadeCliente IN hBoNat001(emitente.cod-emitente, OUTPUT cTipoAtividade, OUTPUT codCnae ).
    RUN retornarDescTipoAtividade(cTipoAtividade, OUTPUT cDescAtividade).
    
   /* MESSAGE cTipoAtividade
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FIND FIRST finalidades_venda 
        WHERE finalidades_venda.cod_finalidade_venda = int(cFinalidade)
        NO-LOCK NO-ERROR.
    RUN buscarNatOperacao IN hBoNat001
        (
            cFinalidade ,       
            fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}  ,             
            emitente.cod-emitente  , 
            INPUT FRAME {&frame-name} fi_cliente_triang ,
            /* '10000'  ,*/
            OUTPUT cNatOperacao,
            OUTPUT iCodParamNatOperacao 
        ).
    RUN retornarErros IN hBoNat001(OUTPUT cErros).
/*     MESSAGE cNatOperacao SKIP              */
/*             iCodParamNatOperacao           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    EXPORT DELIMITER "|"
           fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
           emitente.cod-emitente
           INPUT FRAME {&frame-name} fi_cliente_triang 
           emitente.nome-abrev
           IF emitente.contrib-icms         THEN "SIM"      ELSE "N«O"
            cDescAtividade
           IF AVAIL finalidades_venda       THEN finalidades_venda.DESC_finalidade_venda ELSE ''
           /*IF ext-emitente.LOG_varejo       THEN "SIM"      ELSE "N«O"
           IF ext-emitente.LOG_industria    THEN "SIM"      ELSE "N«O"
           IF ext-emitente.LOG_atacado      THEN "SIM"      ELSE "N«O"
           IF ext-emitente.LOG_servico      THEN "SIM"      ELSE "N«O"*/
           IF lTriangular THEN 'SIM' ELSE 'N«O'
           /*'n∆o se aplica'
           'n∆o se aplica'*/
           replace(cErros, CHR(10) + CHR(13), "|")
           cNatOperacao 
           icodParamNatOperacao
           emitente.estado
           emitente.cgc.
   
END.
OUTPUT CLOSE.
DELETE PROCEDURE hBoNat001.

OS-COMMAND SILENT VALUE('start excel /t t:\especificos\excel\nat_operacao_cliente.xlsx').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarPedidos w-livre 
PROCEDURE buscarPedidos :
/*------------------------------------------------------------------------------
  busca os pedidos, assim como seus itens, conforme os parametros de tela
------------------------------------------------------------------------------*/
DEFINE VARIABLE cNatOperacao            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCodParamNatOperacao    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFinalidade             AS CHAR        NO-UNDO.
DEFINE VARIABLE cTipoAtividade          AS CHAR        NO-UNDO FORMAT "x(8)".
DEFINE VARIABLE codCnae                 AS CHARACTER   NO-UNDO FORMAT "x(8)".
DEFINE VARIABLE cDescAtividade          AS CHARACTER   NO-UNDO FORMAT "x(100)".
/*MESSAGE 'pegadinha do malandro'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
MESSAGE 'quantas vezes ira repetir?'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

RUN esbo/bonat001.p PERSISTENT SET hBoNat001.
OUTPUT TO c:\temp\simulacao_nat_operacao_pedidos.txt.
FOR EACH ped-venda NO-LOCK
    WHERE ped-venda.dt-emissao >= INPUT FRAME {&frame-name} fi_dt_ini
    AND   ped-venda.dt-emissao <= INPUT FRAME {&frame-name} fi_dt_fim
    AND   ped-venda.nr-pedido  >= INPUT FRAME {&frame-name} fi_pedido_ini
    AND   ped-venda.nr-pedido  <= INPUT FRAME {&frame-name} fi_pedido_fim
    AND   ped-venda.cod-sit-ped <> 6.
    FIND FIRST ped-venda-ext 
        WHERE string(ped-venda-ext.nr-pedido) = ped-venda.nr-pedcli
        AND   ped-venda-ext.nome-abrev = ped-venda.nome-abrev NO-LOCK.
    FIND FIRST emitente 
        WHERE emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.
    FIND FIRST ext-emitente
          WHERE  ext-emitente.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
    FIND FIRST nota-fiscal 
        WHERE  nota-fiscal.nome-ab-cli =  ped-venda.nome-abrev 
        AND    nota-fiscal.nr-pedcli   =  ped-venda.nr-pedcli NO-LOCK NO-ERROR.
    /*
    MESSAGE 'vou chamar a natureza de operacao'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    RUN retornarFinalidadeCliente IN hBoNat001(emitente.cod-emitente, OUTPUT cFinalidade, OUTPUT  codCnae).
    RUN retornarTipoAtividadeCliente IN hBoNat001(emitente.cod-emitente, OUTPUT cTipoAtividade, OUTPUT codCnae ).
    RUN retornarDescTipoAtividade(cTipoAtividade, OUTPUT cDescAtividade).
   /* MESSAGE cTipoAtividade
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FIND FIRST finalidades_venda 
        WHERE finalidades_venda.cod_finalidade_venda = int(cFinalidade)
        NO-LOCK NO-ERROR.
    RUN buscarNatOperacao IN hBoNat001
        (
            cFinalidade ,       
            ped-venda.cod-estabel  ,             
            ped-venda.cod-emitente  , 
            ped-venda.nome-abrev-tri,
            /* '10000'  ,*/
            OUTPUT cNatOperacao,
            OUTPUT iCodParamNatOperacao 
        ).
    RUN retornarErros IN hBoNat001(OUTPUT cErros).
    
    EXPORT DELIMITER "|"
           ped-venda.cod-estabel
           ped-venda.nr-pedido 
           ped-venda.cod-emitente
           emitente.nome-abrev
           IF emitente.contrib-icms         THEN "SIM"      ELSE "N«O"
            cDescAtividade
           IF AVAIL finalidades_venda       THEN finalidades_venda.DESC_finalidade_venda ELSE ''
           /*IF ext-emitente.LOG_varejo       THEN "SIM"      ELSE "N«O"
           IF ext-emitente.LOG_industria    THEN "SIM"      ELSE "N«O"
           IF ext-emitente.LOG_atacado      THEN "SIM"      ELSE "N«O"
           IF ext-emitente.LOG_servico      THEN "SIM"      ELSE "N«O"*/
           IF ped-venda-ext.tp-pedido = 'operaá∆o triangular' THEN 'SIM' ELSE 'N«O'
           ped-venda.dt-emissao
           /*'n∆o se aplica'
           'n∆o se aplica'*/
           replace(cErros, CHR(10) + CHR(13), "|")
           cNatOperacao 
           IF AVAIL nota-fiscal THEN string(nota-fiscal.nr-nota-fis) ELSE '0'
           ped-venda.nat-operacao
           IF cNatOperacao <> ped-venda.nat-operacao THEN "DIF" ELSE "OK"
           icodParamNatOperacao
           emitente.estado
           "'" + emitente.cgc.
   
END.
DELETE PROCEDURE hBoNat001.

OS-COMMAND SILENT VALUE('start excel /t t:\especificos\excel\nat_operacao_pedido.xlsx').

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
  DISPLAY fi_dt_ini fi_dt_fim fi_cliente fi_pedido_ini fi_pedido_fim 
          fi_cliente_triang fi_cod_estab 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-2 RECT-1 fi_dt_ini fi_dt_fim fi_cliente fi_pedido_ini 
         fi_pedido_fim fi_cliente_triang fi_cod_estab bt_Simular 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornarDescTipoAtividade w-livre 
PROCEDURE retornarDescTipoAtividade :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT  PARAMETER cAtividade AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cDesc AS CHARACTER   NO-UNDO FORMAT 'X(100)'.
    CASE cAtividade:
        WHEN '1' THEN 
              ASSIGN cDesc = 'Varejo'.
        WHEN '2' THEN
             ASSIGN cDesc = 'Atacado'.
        WHEN '3' THEN
            ASSIGN cDesc = 'Industria'.
        WHEN '4' THEN
            ASSIGN cDesc = 'Serviáo'.
        

    END CASE.

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

