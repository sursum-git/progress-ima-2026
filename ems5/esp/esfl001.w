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
{include/i-prgvrs.i ESFL001 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Parameters Definitions     ---                                       */
/* Local Variable Definitions ---                                       */

{esbo/esbofin601a.i}
{esbo/esbofin271a.i}
{esbo/esbofin490a.i}
{esbo/esbofin533a.i}
{esbo/esboger999.i}
{esbo/boOrdemCompra.i}

DEFINE TEMP-TABLE ttFLuxo LIKE ttTitulo
    FIELD tipo_Registro     AS  CHAR 
    FIELD DESC_tipo_fluxo   AS CHAR FORMAT 'x(100)'
    FIELD num_semana AS INT
    FIELD DESC_Semana AS CHAR FORMAT 'x(30)'
    FIELD desc_fluxo_pai  AS CHAR
    FIELD dia        AS DATE
    FIELD descDia    AS CHAR
    FIELD linha      AS INT
    FIELD LOG_desconsiderar_dia AS LOGICAL 
    FIELD LOG_desconsiderar_semana AS LOGICAL.


DEFINE VARIABLE  h_esbofin601a   AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_esbofin271a   AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_esbofin490a   AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_esboger999    AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_esbofin533a   AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_esbofl010     AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_Acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE  h_boOrdemCompra AS HANDLE      NO-UNDO.

DEFINE VARIABLE vl_saldo            AS DECIMAL     NO-UNDO FORMAT ">>>,>>>,>>9,99".
DEFINE VARIABLE nomeEmpresa         AS CHARACTER  FORMAT 'x(100)' NO-UNDO.
DEFINE VARIABLE vl_saldo_recuperado AS DECIMAL     NO-UNDO EXTENT 2.
DEFINE VARIABLE cHistorico          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE numLinha            AS INTEGER     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 cbEmpresa cbTipoPrevisoes ~
cbDiaIni fiDtIni fiDtFim fiSaldoAnterior fiDtCp fiCodEstabIni fiCodEstabFim ~
btExecutar tgCompras 
&Scoped-Define DISPLAYED-OBJECTS cbEmpresa cbTipoPrevisoes cbDiaIni fiDtIni ~
fiDtFim fiSaldoAnterior fiDtCp fiCodEstabIni fiCodEstabFim tgCompras ~
ed_historico 

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
     LABEL "Gerar Excel" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cbDiaIni AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 6 
     LABEL "Inicio Semana" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Domingo",1,
                     "Segunda",2,
                     "Ter‡a",3,
                     "Quarta",4,
                     "Quinta",5,
                     "Sexta",6,
                     "S bado",7
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbEmpresa AS CHARACTER FORMAT "X(3)":U INITIAL "500" 
     LABEL "Empresa" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "IMA","100",
                     "MED","500"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbTipoPeriodo AS CHARACTER FORMAT "X(1)":U INITIAL "1" 
     LABEL "Tipo Periodo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Semanal","1",
                     "Mensal","2"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbTipoPrevisoes AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 2 
     LABEL "Tipo Previsäes" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Conservadora",1,
                     "Esperada",2,
                     "Otimista",3
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE ed_historico AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 83 BY 3.5 NO-UNDO.

DEFINE VARIABLE fiCodEstabFim AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Estab.Fim" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEstabIni AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab.Ini" 
     VIEW-AS FILL-IN 
     SIZE 15.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtCp AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Inicial CP." 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtFim AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Final" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiSaldoAnterior AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Anterior" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 7.92.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tgCompras AS LOGICAL INITIAL no 
     LABEL "Considerar Ordens de Compra Confirmadas" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     cbEmpresa AT ROW 3 COL 13 COLON-ALIGNED WIDGET-ID 2
     cbTipoPrevisoes AT ROW 3 COL 41 COLON-ALIGNED WIDGET-ID 4
     cbTipoPeriodo AT ROW 4.25 COL 13 COLON-ALIGNED WIDGET-ID 8
     cbDiaIni AT ROW 4.25 COL 41 COLON-ALIGNED WIDGET-ID 10
     fiDtIni AT ROW 5.5 COL 13.29 COLON-ALIGNED WIDGET-ID 12
     fiDtFim AT ROW 5.5 COL 41 COLON-ALIGNED WIDGET-ID 14
     fiSaldoAnterior AT ROW 6.63 COL 41 COLON-ALIGNED WIDGET-ID 18
     fiDtCp AT ROW 6.67 COL 13.29 COLON-ALIGNED WIDGET-ID 16
     fiCodEstabIni AT ROW 7.75 COL 13.57 COLON-ALIGNED WIDGET-ID 30
     fiCodEstabFim AT ROW 7.75 COL 41 COLON-ALIGNED WIDGET-ID 32
     btExecutar AT ROW 9.17 COL 71.72 WIDGET-ID 6
     tgCompras AT ROW 9.5 COL 15.29 WIDGET-ID 26
     ed_historico AT ROW 11.17 COL 5 NO-LABEL WIDGET-ID 22
     "Hist¢rico Saldo" VIEW-AS TEXT
          SIZE 23 BY .54 AT ROW 10.5 COL 5 WIDGET-ID 24
          FONT 0
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.58 COL 5 WIDGET-ID 28
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
         TITLE              = "Fluxo de Caixa"
         HEIGHT             = 14
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
/* SETTINGS FOR COMBO-BOX cbTipoPeriodo IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbTipoPeriodo:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR EDITOR ed_historico IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Fluxo de Caixa */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Fluxo de Caixa */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
 RUN matarBos. 
 APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecutar w-livre
ON CHOOSE OF btExecutar IN FRAME f-cad /* Gerar Excel */
DO:    
   IF INPUT FRAME {&FRAME-NAME} fiDtIni < TODAY THEN DO:
   
      MESSAGE 'A Data de inicio nÆo pode ser inferior a data atual'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fiDtIni.
      RETURN NO-APPLY.  
   END.

   IF INPUT FRAME {&FRAME-NAME} fiDtFim  < INPUT FRAME {&FRAME-NAME} fiDtIni  THEN DO:
      MESSAGE 'A data final nÆo pode ser menor que a data inicial'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fiDtFim.
      RETURN NO-APPLY.  
   END.

   IF fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''  THEN DO:
      MESSAGE 'Informe a data final'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fiDtFim.
      RETURN NO-APPLY.  
   END.

   IF fiDtIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''  THEN DO:
      MESSAGE 'Informe a data Inicial'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fiDtIni.
      RETURN NO-APPLY.  
   END.

   RUN utp\ut-acomp.p       persistent set h_acomp.
   /*RUN esbo\esbofin533a.p PERSISTENT SET  h_esbofin533a.*/
   RUN esbo\esbofin601a.p   PERSISTENT SET  h_esbofin601a .
   RUN esbo\esbofin271a.p   PERSISTENT SET  h_esbofin271a .
   RUN esbo\esbofin490a.p   PERSISTENT SET  h_esbofin490a .
   RUN esbo\esboger999.p    PERSISTENT SET  h_esboger999.  
   RUN esbo\esbofin533a.p   PERSISTENT SET  h_esbofin533a.
   RUN esbo\boOrdemCompra.p PERSISTENT SET  h_boOrdemCompra.


   RUN limpartabelasTemporarias.
   
   IF vl_saldo_recuperado[1] <>  INPUT FRAME {&FRAME-NAME} fiSaldoAnterior THEN
      RUN atualizarSaldo IN h_esbofl010(INPUT FRAME {&frame-name} fiDtIni,
                                     cbEmpresa:SCREEN-VALUE,
                                     1,
                                     INPUT FRAME {&FRAME-NAME} fiSaldoAnterior
                                     ).

   RUN pi-inicializar IN h_acomp(INPUT "Acompanhamento - Busca dos Dados").
   
   /************ buscar saldo de contas correntes**********/
   RUN pi-acompanhar IN h_acomp(INPUT "Buscando Saldos dos Bancos ").

 /* RUN definirDataCorte  IN h_esbofin533a(TODAY).
  RUN buscarSaldos      IN h_esbofin533a.
  RUN retornarRegistros IN h_esbofin533a(OUTPUT TABLE ttSaldo).
  RUN retornarSaldoAtual IN h_esbofin533a(OUTPUT vl_Saldo).
  IF VALID-HANDLE(h_esbofin533a) THEN
     DELETE PROCEDURE h_esbofin533a.*/

  ASSIGN vl_saldo = INPUT FRAME {&FRAME-NAME} fiSaldoAnterior.


  /*********** buscar ordens confirmadas no compras ******/
  IF INPUT FRAME {&frame-name} tgCompras  = YES THEN DO:
     RUN pi-acompanhar        IN h_acomp(INPUT " Buscando Ordens de Compras   "). 
     RUN limparDados          IN h_BoOrdemCompra.
     RUN definirSituacaoOrdem IN h_boOrdemCompra(2,2).
     RUN definirEstab         IN h_boOrdemCOmpra(fiCodEstabIni:SCREEN-VALUE,fiCodEstabFim:SCREEN-VALUE).
     RUN buscarOrdensCompra   IN h_boOrdemCompra(YES).
     RUN definirDataVencto    IN h_BoOrdemCompra (01.01.2001,fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
     RUN filtrarDtVencto      IN h_boOrdemCompra.
     RUN retornarttPrazo      IN h_boOrdemCompra(OUTPUT TABLE ttPrazo).
  END.
   
   /*********** buscar valores a receber em aberto ******/
   RUN pi-acompanhar        IN h_acomp(INPUT "Buscando Valores a Receber  ").
   RUN definirEmpresa       IN h_esbofin490a(cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   RUN definirEstab         IN h_esbofin490a(fiCodEstabIni:SCREEN-VALUE,fiCodEstabFim:SCREEN-VALUE).
   RUN definirDataLimite    IN h_Esbofin490a(fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   RUN buscarTitulos        IN h_esbofin490a.
   RUN retornarRegistros    IN h_esbofin490a(OUTPUT TABLE ttTitulo).
   
   

   /********** buscar valores a pagar em aberto  ********/
   RUN pi-acompanhar        IN h_acomp(INPUT "Buscando Valores a Pagar ").
   RUN definirEmpresa       IN h_esbofin271a(cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   RUN definirEstab         IN h_esbofin271a(fiCodEstabIni:SCREEN-VALUE,fiCodEstabFim:SCREEN-VALUE).
   RUN definirDataInicio    IN h_esbofin271a(INPUT FRAME {&frame-name} fiDtCp).
   RUN definirDataLimite    IN h_esbofin271a(fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   RUN buscarTitulos        IN h_esbofin271a.
   RUN retornarRegistros    IN h_esbofin271a(OUTPUT TABLE ttTituloAP).
  
    /********** buscar previsoes no periodo ***********/
    /*IF cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '100'  THEN
       RUN definirEstab IN h_esbofin601a('101').

    IF cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '500'  THEN
       RUN definirEstab IN h_esbofin601a('501').*/ 

   RUN definirIntervalEstab IN h_esbofin601a(fiCodEstabIni:SCREEN-VALUE,
                                             fiCodEstabFim:SCREEN-VALUE).



    RUN pi-acompanhar IN h_acomp(INPUT "Buscando Previsäes do Fluxo de Caixa").
    /*RUN definirEmpresa  IN h_esbofin601a(cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    A defini‡Æo da empresa teve que ser retirada, pois, na verdade o campo empresa est  ficando em branco quando da inclusÆo diretamente pelo fluxo.
    */
    RUN definirDatas    IN h_esbofin601a(INPUT FRAME {&FRAME-NAME} fiDtIni,INPUT FRAME {&FRAME-NAME} fiDtFim).
    RUN definirFluxo    IN h_esbofin601a(INPUT FRAME {&frame-name} cbTipoPrevisoes).
    RUN buscarPrevisoes IN h_esbofin601a.
    RUN retornarRegistros IN h_esbofin601a(OUTPUT TABLE ttPrevisao).
   
    /********** buscar Semanas ************************/
    RUN pi-acompanhar       IN h_acomp(INPUT "Calculando Semanas").
    RUN definirDataIni      IN h_esboger999(INPUT FRAME {&frame-name} fiDtIni).
    RUN definirDataFim      IN h_esboger999(INPUT FRAME {&frame-name} fiDtFim).
    RUN definirInicioSemana IN h_esboger999(INPUT FRAME {&frame-name} cbDiaIni).
    RUN calcularSemanas     IN h_esboger999.
    RUN retornarRegistros   IN h_esboger999(OUTPUT TABLE ttSemana).
    
    ASSIGN numLinha = 0.
    /************************** Criar Tabela Temporaria unificada a partir do retorno das demais ************************************/
    RUN pi-acompanhar IN h_acomp('Agrupando Dados....').

    FOR EACH ttPrazo:

        RUN criarTtFluxo(
                       cbEmpresa:SCREEN-VALUE ,
                       ttPrazo.codEstab       ,
                       string(ttPrazo.numOrdem)  ,
                       string(ttPrazo.parcela)   ,
                       ''            ,
                       STRING(ttPrazo.numPedido) ,
                       ttPrazo.Fornec            ,
                       ttPrazo.descFornec        ,
                       ttPrazo.dataEntrega       ,
                       ttPrazo.dtVenc            ,
                       ttPrazo.dtVenc            ,
                       ttPrazo.preco  * -1       ,
                       ttPrazo.preco  * -1       ,
                       NO                        ,
                       ttPrazo.codDespesa        ,
                       '03-Compras'                 ,
                       0                         ,
                       ''                        ,
                       0                         ,
                       ''                        ,
                       descDespesa               ,
                       NO                        ,
                       NO 
            ).
    END.

    FOR EACH ttTitulo:
/*         FIND FIRST tip_fluxo_financ                                                             */
/*             WHERE tip_fluxo_financ.cod_tip_fluxo_financ = ttTItulo.tipo_fluxo NO-LOCK NO-ERROR. */
            RUN criarTtFluxo(ttTitulo.cod_empresa      ,
                          ttTitulo.cod_estab           ,
                          ttTitulo.cod_tit_acr         ,
                          ttTitulo.cod_parcela         ,
                          ttTitulo.cod_ser_docto       ,
                          ttTitulo.cod_espec_docto     ,
                          ttTitulo.cdn_cliente         ,
                          ttTitulo.nom_abrev           ,
                          ttTitulo.dat_emis_docto      ,
                          ttTitulo.dat_vencto_tit_acr  ,
                          ttTitulo.dat_fluxo_tit_acr   ,
                          ttTitulo.val_origin_tit_acr ,
                          ttTitulo.val_sdo_tit_acr    ,
                          ttTitulo.situacao            ,
                          ttTitulo.tipo_fluxo          ,
                          '01-A Receber',
                           0,
                           '',
                            ?,
                           '' ,
                           '' ,
                             NO,
                             NO  ).
    END.

    FOR EACH ttTituloAP:
/*         FIND FIRST tip_fluxo_financ                                                               */
/*             WHERE tip_fluxo_financ.cod_tip_fluxo_financ = ttTItuloAP.tipo_fluxo NO-LOCK NO-ERROR. */
        RUN criarTTfluxo( ttTituloAP.cod_empresa ,
            ttTituloAP.cod_estab             ,
            ttTituloAP.cod_tit_ap            ,
            ttTituloAP.cod_parcela           ,
            ttTituloAP.cod_ser_docto         ,
            ttTituloAP.cod_espec_docto       ,
            ttTItuloAP.cdn_fornecedor        ,
            ttTituloAP.nom_abrev             ,
            ttTituloAP.dat_emis_docto        ,
            ttTituloAP.dat_vencto_tit_ap     ,
            ttTituloAP.dat_vencto_tit_ap     ,  
            ttTituloAP.val_origin_tit_ap     ,
            ttTituloAP.val_sdo_tit_ap        ,
            ttTituloAP.situacao              ,
            ttTituloAP.tipo_fluxo            ,
            '02-A Pagar',
            0,
            '',
            ?,
            '',
            '',
             NO,
             NO). 
    END.

    FOR EACH ttPrevisao:
          RUN criarTTfluxo(IF SUBSTR(ttPrevisao.cod_Estab,1,1) = '1' THEN '100' ELSE '500' ,
            ttPrevisao.cod_estab             ,
            ttPrevisao.cod_tip_fluxo_financ  ,
            '' ,
            string(ttPrevisao.num_Fluxo_Cx)  ,
            ''    ,
            ''    ,
            ttPrevisao.des_histor_movto_fluxo_cx ,
            ttPrevisao.dat_movto_fluxo_cx        ,
            ttPrevisao.dat_movto_fluxo_cx        ,
            ttPrevisao.dat_movto_fluxo_cx        ,  
            ttPrevisao.val_movto_fluxo_cx        ,
            ttPrevisao.val_movto_fluxo_cx        ,
            NO    ,
            ttPrevisao.cod_tip_fluxo_financ      ,
            '04-Previsäes',
            0,
            '',
            ?,
            '',
            '',
            NO,
            NO  ). 
    END.

    RUN atribuirSemana.
    RUN atribuirDia.
    RUN calcularSaldosIniciais.

    RUN pi-finalizar IN h_acomp.
    /***************** exporta‡Æo de registros *********************************************/

    OUTPUT TO c:\temp\ParamsFLuxo.txt.
    ASSIGN nomeEmpresa = IF cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '100' THEN 'IMA' ELSE 'MED'.
    /*MESSAGE  fiDtIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}            
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    PUT  'Empresa:'  nomeEmpresa '- Estab Ini:' fiCodEstabIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} ' - Estab Fim:' fiCodEstabFim:SCREEN-VALUE IN FRAME {&FRAME-NAME}  SKIP
          'Data Inicial:' fiDtIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} FORMAT 'x(12)'            
        ' - Data Final:' fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME} FORMAT 'x(12)'            SKIP
        ' Saldo Informado:' vl_Saldo FORMAT ">>>,>>>,>>9.99" .

    OUTPUT CLOSE.
    
    OUTPUT TO c:\temp\ttTitulo.txt.
    FOR EACH ttTitulo:
        EXPORT DELIMITER "|" ttTitulo.
    END.
    OUTPUT CLOSE.

    OUTPUT TO c:\temp\ttTituloAP.txt.
    FOR EACH ttTituloAP:
        EXPORT DELIMITER "|" ttTituloAP.
    END.
    OUTPUT CLOSE.

    OUTPUT TO c:\temp\ttSaldo.txt.
    FOR EACH ttSaldo:
        EXPORT DELIMITER "|" ttSaldo.
    END.
    OUTPUT CLOSE.

    OUTPUT TO c:\temp\ttPrevisao.txt.
    FOR EACH ttPrevisao:
        EXPORT DELIMITER "|" ttPrevisao.
    END.
    OUTPUT CLOSE.

    OUTPUT TO c:\temp\ttSemanas.txt.
    FOR EACH ttSemana:
        EXPORT DELIMITER "|" ttSemana.
    END.
    OUTPUT CLOSE.
    

    OUTPUT TO c:\temp\ttFLuxo.txt.
    FOR EACH ttFLuxo.
        EXPORT DELIMITER "|" ttFLuxo.
    END.
    OUTPUT CLOSE.

    OS-COMMAND  SILENT value("START excel /t t:\especificos\excel\fluxo_caixa.xlsx") .
   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiDtFim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDtFim w-livre
ON LEAVE OF fiDtFim IN FRAME f-cad /* Dt.Final */
DO:
  RUN piRetornarDadosSaldo.
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
             cbEmpresa:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE atribuirDia w-livre 
PROCEDURE atribuirDia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO INIT 0.
FOR EACH ttFLuxo
    WHERE tipo_Registro <> 'saldo'
    BREAK BY ttFluxo.dat_vencto_tit_acr :
    
    IF FIRST-OF(ttFluxo.dat_vencto_tit_acr) THEN DO:
       IF ttFluxo.dat_vencto_tit_acr >= INPUT FRAME {&FRAME-NAME} fiDtIni THEN DO:
          ASSIGN iCont = iCont + 1.                   
          CREATE ttDia.
          ASSIGN ttDia.ordem = iCont
                 ttDia.data  = ttFluxo.dat_vencto_tit_acr. 
       END.
    END.
    FIND FIRST ttDia
        WHERE ttDia.data = ttFluxo.dat_vencto_tit_acr NO-LOCK NO-ERROR.
    IF NOT AVAIL ttDia THEN
       ASSIGN ttFluxo.descDia = ' vencido'
              ttFluxo.dia     = ttFluxo.dat_vencto_tit_acr .
    ELSE 
       ASSIGN ttFluxo.descDia = string(ttDia.ordem,'999') + '-' +  STRING(ttFluxo.dat_vencto_tit_acr,'99/99/9999')
              ttFluxo.dia    =  ttFluxo.dat_vencto_tit_acr.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AtribuirSemana w-livre 
PROCEDURE AtribuirSemana :
/*------------------------------------------------------------------------------
  atribuir Semana aos registros
------------------------------------------------------------------------------*/
FOR EACH ttFLuxo
    WHERE tipo_Registro <> 'saldo':
    FIND FIRST ttSemana 
        WHERE ttFluxo.dat_vencto_tit_acr >= ttSemana.diaInicial
        AND   ttFLuxo.dat_vencto_Tit_acr <= ttSemana.diaFInal NO-ERROR.
   ASSIGN ttFLuxo.num_semana = IF AVAIL ttSemana THEN ttSemana.ordem ELSE 0
          ttFLuxo.desc_semana = IF AVAIL ttSemana THEN 
              /*STRING(ttSemana.ordem) + ' Semana ' + CHR(10) + CHR(13) +*/
                string(ttSemana.diaInicial,'99/99/9999') + ' - ' + string(ttSemana.diaFinal,'99/99/9999') ELSE 'Vencido'.
    
   

END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcularSaldosIniciais w-livre 
PROCEDURE calcularSaldosIniciais :
/*------------------------------------------------------------------------------
calcula os saldo iniciais
------------------------------------------------------------------------------*/
DEFINE VARIABLE dTotal      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotalIni   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotDia AS DECIMAL     NO-UNDO.
DEFINE BUFFER bfFluxo FOR ttFLuxo.
DEFINE BUFFER bfDia    FOR ttDia.


OUTPUT TO c:\temp\vl_saldo_semana.txt.
PUT "valor saldo anterior bancos:" vl_saldo SKIP.
OUTPUT CLOSE.
FOR EACH ttSemana:
    OUTPUT TO VALUE('c:\temp\semana-' + STRING(ttSemana.ordem) + ".txt").
    ASSIGN dTotal = 0.
    FOR EACH bfFLuxo 
        WHERE bfFLuxo.num_semana = ttSemana.ordem - 1
        /*AND bfFluxo.tipo_registro = 'Saldo'*/ :

        /*na semana vencida s¢ considera o saldo anterior*/
        IF  /*bfFluxo.tipo_registro <> 'Saldo' AND*/ bfFLuxo.num_semana = 0 THEN NEXT.
        EXPORT  DELIMITER "|" bfFluxo.
        ASSIGN dTotal =  dTotal + bfFLuxo.val_sdo_tit_acr.
    END.
    ASSIGN ttSemana.vlFinal = dTotal.
    EXPORT DELIMITER "|" ttSemana.
    IF ttSemana.ordem = 2 THEN
       ASSIGN dTotal = dTotal + vl_saldo.
    OUTPUT CLOSE.
    /*IF ttSEmana.ordem = 1  THEN
           ASSIGN dTotalIni = dTotal + vl_saldo.*/

      RUN criarTtFluxo(cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}      ,
                          '',
                          'calculado'        ,
                          ''                 ,
                          ''                 ,
                          ''                 ,
                          0                  ,
                          ''                 ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni              ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni              ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni              ,
                          0                  ,
                          dTotal             ,
                          NO                 ,
                          '00'    ,
                          '00-Saldo',
                          ttSemana.ordem     ,
                          string(ttSemana.diaInicial,'99/99/9999') + ' - ' + string(ttSemana.diaFinal,'99/99/9999')  ,
                          ?,
                          'desconsiderar',
                          'Saldo Inicial',
                          YES,
                          NO).

END.

OUTPUT TO c:\temp\ttDia.csv.
FOR EACH ttDia:
    ASSIGN dTotal = 0.
    //PUT "data;" ttDia.data SKIP.
    FOR EACH bfFluxo
        WHERE bfFluxo.dat_vencto_tit_acr = ttDia.data
        AND bfFluxo.tipo_registro  <> 'saldo'
        AND bfFluxo.descDia <> 'desconsiderar':
        //IF bfFluxo.dat_vencto_tit_acr < TODAY THEN NEXT.

        

        /*na semana vencida s¢ considera o saldo anterior*/
        IF   ttDia.ordem = 0 THEN NEXT.
        /*MESSAGE Dtotal SKIP
                bfFluxo.val_sdo_tit_acr SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
        ASSIGN dTotal =  dTotal + bfFluxo.val_sdo_tit_acr.
        //EXPORT DELIMITER ";" bfFluxo.val_sdo_tit_acr bfFluxo.linha .   
    END.
    ASSIGN ttDia.vlFinal = dTotal.
    //PUT "total do dia - " ttDia.data dTotal SKIP. 
    EXPORT DELIMITER "|" ttDia.
    
   /* IF ttDia.ordem = 2 THEN
       ASSIGN dTotal = dTotal + vl_saldo.*/
END.
OUTPUT CLOSE.

/**cria‡Æo do saldo por dia */
OUTPUT TO c:\temp\saldottdia.txt.
FOR EACH ttDia 
    WHERE ttDia.ordem > 1 BY ttDia.data:
    EXPORT DELIMITER "|" ttDia.
    ASSIGN dTotDia = 0.
    FOR EACH bfDia
        WHERE bfDia.data < ttDia.data.
        ASSIGN dTotDia = dTotDia + bfDia.vlFinal.
    END.
    IF ttDia.ordem >= 2 THEN
           ASSIGN dTotDia = dTotDia + vl_saldo .

    

    RUN criarTtFluxo(     cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}      ,
                          IF SUBSTR(cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,1)  = '1' THEN '101' ELSE '501' ,
                          'calculado'   ,
                          ''            ,
                          ''            ,
                          ''            ,
                          0             ,
                          ''            ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni         ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni         ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni         ,
                          0             ,
                          dTotDia       ,
                          NO            ,
                          '00'          ,
                          '00-Saldo'    ,
                           0            ,
                           'desconsiderar' ,
                            ttDia.data  ,
                           IF ttDia.data  < INPUT FRAME {&FRAME-NAME} fiDtIni THEN ' vencido' ELSE  string(ttDia.ordem,'999') + '-' + STRING(ttDia.data,'99/99/9999') ,
                           'Saldo Inicial' ,
                            NO ,
                            YES ).



END.
OUTPUT CLOSE.

FIND FIRST ttSemana 
    WHERE ttSemana.ordem = 1 NO-ERROR.
FIND FIRST ttDia
    WHERE ttDia.ordem = 1 NO-ERROR.
/*MESSAGE AVAIL ttDia
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

RUN criarTtFluxo(     cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME}      ,
                          IF SUBSTR(cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,1)  = '1' THEN '101' ELSE '501' ,
                         'informado'    ,
                         ''             ,
                         ''             ,
                         ''             ,
                         0              ,
                         ''             ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni  - 1   ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni  - 1   ,
                          INPUT FRAME {&FRAME-NAME} fiDtIni  - 1   ,
                          0             ,
                          vl_saldo      ,
                          NO            ,
                          '00'          ,
                          '00-Saldo'    ,
                           1            ,
                           IF AVAIL ttSemana THEN string(ttSemana.diaInicial,'99/99/9999') + ' - ' + string(ttSemana.diaFinal,'99/99/9999') ELSE '',
                           IF AVAIL ttDia THEN  ttDia.data ELSE ?,
                           IF AVAIL ttDia THEN STRING(ttDia.ordem,'999') + '-' +  STRING(ttDia.data,'99/99/9999') ELSE '' ,
                           'Saldo Inicial',
                           NO,
                           NO ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criarTtFluxo w-livre 
PROCEDURE criarTtFluxo :
DEFINE INPUT  PARAMETER codEmpresa              AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER codEstab                AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER codTitAcr               LIKE ttFluxo.cod_tit_acr  NO-UNDO.
DEFINE INPUT  PARAMETER cParcela                AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER codSerDocto             AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER codEspecDocto           AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER codEmitente             AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER nomAbrev                LIKE ttFluxo.nom_abrev   NO-UNDO.
DEFINE INPUT  PARAMETER datEmisDocto            AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER datVenctoTitAcr         AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER datFluxoTitAcr          AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER valOriginTitAcr         AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER valSdoTitAcr            AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER lSituacao               AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER tipoFluxo               AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER tipoRegistro            AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER numSemana               AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER descSemana              AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER dDia                    AS DATE    NO-UNDO.
DEFINE INPUT  PARAMETER cDescDia                AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER cDescTipoFluxo          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER logDesconsiderarDia     AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER logDesconsiderarSemana  AS LOGICAL     NO-UNDO.


ASSIGN numLinha                   = numLinha + 1.
CREATE ttFLuxo.
ASSIGN 
ttFluxo.linha               = numLinha
ttFLuxo.cod_empresa         = codEmpresa
ttFLuxo.cod_estab           = codEStab 
ttFLuxo.cod_tit_acr         = codTitAcr
ttfluxo.cod_parcela         = cParcela
ttFLuxo.cod_ser_docto       = codSerDocto
ttFluxo.cod_espec_docto     = codEspecDocto
ttFluxo.cdn_cliente         = codEmitente
ttFLuxo.nom_abrev           = nomAbrev
ttFLuxo.dat_emis_docto      = datEmisDocto
ttFLuxo.dat_vencto_tit_acr  = datVenctoTitAcr
ttFLuxo.dat_fluxo_tit_acr   = datFluxoTitAcr 
ttFLuxo.val_origin_tit_acr  = valOriginTitAcr
ttFLuxo.val_sdo_tit_acr     = valSdoTitAcr
ttFLuxo.situacao            = lSituacao 
ttFLuxo.tipo_Fluxo          = tipoFluxo
ttFLuxo.tipo_registro       = tipoRegistro
ttFluxo.num_Semana          = numSemana    
ttFluxo.DESC_semana         = descSemana
ttFluxo.dia                 = dDia
ttFluxo.descDia             = cDescDia
ttFluxo.DESC_tipo_fluxo     = cDescTipoFluxo 
ttFluxo.LOG_desconsiderar_dia    = logDesconsiderarDia
ttFluxo.LOG_desconsiderar_semana = logDesconsiderarSemana.


RUN retornarDescFLuxo(ttFLuxo.tipo_Fluxo, OUTPUT ttFLuxo.desc_Tipo_fluxo).
RUN retornarDescFLuxoPai(ttFLuxo.tipo_Fluxo, OUTPUT ttFLuxo.desc_fluxo_pai).

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
  DISPLAY cbEmpresa cbTipoPrevisoes cbDiaIni fiDtIni fiDtFim fiSaldoAnterior 
          fiDtCp fiCodEstabIni fiCodEstabFim tgCompras ed_historico 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 cbEmpresa cbTipoPrevisoes cbDiaIni fiDtIni fiDtFim 
         fiSaldoAnterior fiDtCp fiCodEstabIni fiCodEstabFim btExecutar 
         tgCompras 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gerarExcel w-livre 
PROCEDURE gerarExcel :
/*------------------------------------------------------------------------------
  gerar Excel com as informa‡äes do Fluxo de Caixa  
------------------------------------------------------------------------------*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE limpartabelasTemporarias w-livre 
PROCEDURE limpartabelasTemporarias :
/*------------------------------------------------------------------------------
  limpar todas as tabelas temporarias
------------------------------------------------------------------------------*/
RUN limparDados in  h_esbofin601a .
RUN limparDados in  h_esbofin271a .
RUN limparDados in  h_esbofin490a .
RUN limparDados in  h_esboger999.  
RUN limparDados in  h_esbofin533a.
EMPTY TEMP-TABLE ttTitulo.
EMPTY TEMP-TABLE ttTituloAP.
EMPTY TEMP-TABLE ttPrevisao.
EMPTY TEMP-TABLE ttFluxo.
EMPTY TEMP-TABLE ttSaldo.
EMPTY TEMP-TABLE ttPrazo.
EMPTY TEMP-TABLE ttSemana.
EMPTY TEMP-TABLE ttdia.



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

  {utp/ut9000.i "ESFL001" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fiDtIni:SCREEN-VALUE = STRING(TODAY,'99/99/9999')
         fiDtCP:SCREEN-VALUE  = STRING(TODAY,'99/99/9999').
  RUN esbo\esbofl010.p   PERSISTENT SET  h_esbofl010.
  run pi-after-initialize.

   /*IF c-seg-usuario = 'super' THEN
     ASSIGN tgCompras:SENSITIVE IN FRAME {&FRAME-NAME} = YES.*/
  
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available w-livre 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

 

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE matarBOs w-livre 
PROCEDURE matarBOs :
IF VALID-HANDLE(h_esbofl010) THEN
      DELETE PROCEDURE h_esbofl010.

IF VALID-HANDLE(h_esbofin490a) THEN
     DELETE PROCEDURE h_esbofin490a.

IF VALID-HANDLE(h_esbofin271a) THEN
     DELETE PROCEDURE h_esbofin271a.

IF VALID-HANDLE(h_esbofin601a) THEN
     DELETE PROCEDURE h_esbofin601a.

IF VALID-HANDLE(h_esboger999) THEN
       DELETE PROCEDURE h_esboger999.

IF VALID-HANDLE(h_boOrdemCompra) THEN
    DELETE PROCEDURE h_boOrdemCompra.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piRetornarDadosSaldo w-livre 
PROCEDURE piRetornarDadosSaldo :
IF cbempresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN DO:
      MESSAGE  'Favor informar a Empresa primeiro'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.      
      APPLY 'entry' TO cbEmpresa IN FRAME {&FRAME-NAME}.
      ASSIGN fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
      RETURN NO-APPLY .
   END.
   RUN buscarSaldo IN h_esbofl010(INPUT FRAME {&FRAME-NAME} fiDtIni,
                                 INPUT FRAME {&FRAME-NAME} cbEmpresa,
                                 OUTPUT vl_saldo_recuperado[1],
                                 OUTPUT vl_saldo_recuperado[2]  ).
  RUN buscarHistorico IN h_esbofl010(INPUT FRAME {&FRAME-NAME} fiDtIni,
                                 INPUT FRAME {&FRAME-NAME} cbEmpresa,
                                 OUTPUT cHistorico ).
  ASSIGN fiSaldoAnterior:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vl_saldo_recuperado[1],'->>>,>>>,>>9.99')
         ed_historico:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cHistorico.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornarDescFluxo w-livre 
PROCEDURE retornarDescFluxo :
/*------------------------------------------------------------------------------
Retorna a descri‡Æo do fluxo de caixa PAI do tipo de fluxo passado por parametro
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pTipoFluxo AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pDescFluxo AS CHARACTER   NO-UNDO FORMAT 'x(50)'.

ASSIGN pDescFluxo = ''.

FIND FIRST tip_fluxo_financ NO-LOCK
    WHERE tip_fluxo_financ.cod_tip_fluxo_financ = pTipoFluxo NO-ERROR.
IF AVAIL tip_fluxo_Financ THEN DO:
    ASSIGN pDescFLuxo = string(tip_fluxo_financ.cod_tip_fluxo_financ) + ' - ' +  tip_fluxo_financ.des_tip_fluxo_financ.
END.
ELSE DO: 
   FIND FIRST trad_fluxo_ext
       WHERE trad_fluxo_ext.cod_fluxo_financ_ext = pTipoFluxo
       NO-LOCK NO-ERROR.
   IF AVAIL trad_fluxo_ext THEN DO:
       FIND FIRST tip_fluxo_financ NO-LOCK
           WHERE tip_fluxo_financ.cod_tip_fluxo_financ = trad_fluxo_ext.cod_tip_fluxo_financ NO-ERROR.
       IF AVAIL tip_fluxo_Financ THEN DO:
           ASSIGN pDescFLuxo = string(tip_fluxo_financ.cod_tip_fluxo_financ) + ' - ' +  tip_fluxo_financ.des_tip_fluxo_financ.
       END.
   END.    
END.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornarDescFluxoPai w-livre 
PROCEDURE retornarDescFluxoPai :
/*------------------------------------------------------------------------------
Retorna a descri‡Æo do fluxo de caixa PAI do tipo de fluxo passado por parametro
------------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER pTipoFluxo AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER pDescFluxo AS CHARACTER   NO-UNDO FORMAT 'x(50)'.

FIND FIRST estrut_tip_fluxo_financ 
    WHERE estrut_tip_fluxo_financ.cod_tip_fluxo_financ_filho = pTipoFLuxo NO-LOCK NO-ERROR.
IF AVAIL estrut_tip_fluxo_financ THEN DO:
    FIND FIRST  tip_fluxo_financ NO-LOCK
        WHERE tip_fluxo_financ.cod_tip_fluxo_financ = estrut_tip_fluxo_financ.cod_tip_fluxo_financ_pai NO-ERROR.
    IF AVAIL tip_fluxo_financ THEN
       ASSIGN pDescFLuxo = string(tip_fluxo_financ.cod_tip_fluxo_financ) + ' - ' +  tip_fluxo_financ.des_tip_fluxo_financ.      
    ELSE
        ASSIGN pDescFluxo = ''.
END.
ELSE 
   ASSIGN pDescFLuxo = ''.


END PROCEDURE.
/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_tip_fluxo_financ_pai         char        im
   20 num_seq_estrut_tip_fluxo         inte        im
   30 cod_tip_fluxo_financ_filho       char        im
   40 dat_inic_valid                   date        m
   50 dat_fim_valid                    date        m
   60 ind_tip_secao_fluxo_cx           char        m
   70 cod_livre_1                      char
   80 log_livre_1                      logi
   90 num_livre_1                      inte
  100 val_livre_1                      deci-4
  110 dat_livre_1                      date
  120 num_clas_tip_fluxo_financ        inte        im
  130 cod_livre_2                      char
  140 dat_livre_2                      date
  150 log_livre_2                      logi
  160 num_livre_2                      inte
  170 val_livre_2                      deci-4
  180 cdd_version                      deci-0
*/

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

