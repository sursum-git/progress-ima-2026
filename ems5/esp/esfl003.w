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
{include/i-prgvrs.i ESFL003 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Parameters Definitions     ---                                       */
/* Local Variable Definitions ---                                       */

{esbo/bo_ctbl_cx.i}



DEFINE VARIABLE hBoCtblCx AS HANDLE      NO-UNDO.


DEFINE VARIABLE nomeEmpresa         AS CHARACTER  FORMAT 'x(100)' NO-UNDO.
DEFINE VARIABLE cHistorico          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE numLinha            AS INTEGER     NO-UNDO.
DEFINE VARIABLE h_Acomp              AS HANDLE      NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 fiCodEstabIni fiCodEstabFim ~
fiDtIni fiDtFim fiCtaCorren fiNrSeq tgMsg btExecutar 
&Scoped-Define DISPLAYED-OBJECTS fiCodEstabIni fiCodEstabFim fiDtIni ~
fiDtFim fiCtaCorren fiNrSeq tgMsg 

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
     SIZE 13.86 BY 1.08
     FONT 1.

DEFINE VARIABLE cbEmpresa AS CHARACTER FORMAT "X(3)":U INITIAL "500" 
     LABEL "Empresa" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "IMA","100",
                     "MED","500"
     DROP-DOWN-LIST
     SIZE 9.57 BY 1 NO-UNDO.

DEFINE VARIABLE fiCodEstabFim AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" 
     LABEL "Estab.Fim" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEstabIni AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab.Ini" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiCtaCorren AS CHARACTER FORMAT "X(25)":U 
     LABEL "Cta.Corrente" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtFim AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Final" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15.72 BY .79 NO-UNDO.

DEFINE VARIABLE fiNrSeq AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Sequància" 
     VIEW-AS FILL-IN 
     SIZE 5.14 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 4.67.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tgMsg AS LOGICAL INITIAL no 
     LABEL "Mostrar Mensagens" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     cbEmpresa AT ROW 3.38 COL 24 COLON-ALIGNED WIDGET-ID 2
     fiCodEstabIni AT ROW 3.38 COL 24.14 COLON-ALIGNED WIDGET-ID 30
     fiCodEstabFim AT ROW 3.38 COL 52.57 COLON-ALIGNED WIDGET-ID 32
     fiDtIni AT ROW 4.67 COL 24 COLON-ALIGNED WIDGET-ID 12
     fiDtFim AT ROW 4.67 COL 52.72 COLON-ALIGNED WIDGET-ID 14
     fiCtaCorren AT ROW 5.96 COL 24 COLON-ALIGNED WIDGET-ID 34
     fiNrSeq AT ROW 6 COL 52.86 COLON-ALIGNED WIDGET-ID 36
     tgMsg AT ROW 6 COL 62 WIDGET-ID 38
     btExecutar AT ROW 7.33 COL 5.14 WIDGET-ID 6
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
         TITLE              = "Fluxo de Caixa Realizado"
         HEIGHT             = 7.92
         WIDTH              = 90.29
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
/* SETTINGS FOR COMBO-BOX cbEmpresa IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbEmpresa:HIDDEN IN FRAME f-cad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Fluxo de Caixa Realizado */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    RUN matarBos. 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Fluxo de Caixa Realizado */
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
   IF INPUT FRAME {&FRAME-NAME} fiDtFim > TODAY THEN DO:
   
      MESSAGE 'A Data Final n∆o pode ser superior a data atual'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fiDtIni.
      RETURN NO-APPLY.  
   END.

   IF INPUT FRAME {&FRAME-NAME} fiDtFim  < INPUT FRAME {&FRAME-NAME} fiDtIni  THEN DO:
      MESSAGE 'A data final n∆o pode ser menor que a data inicial'
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
   RUN limpartabelasTemporarias.

   RUN pi-inicializar IN h_acomp(INPUT "Acompanhamento - Busca dos Dados").
   RUN pi-acompanhar  IN h_acomp(INPUT "Extraindo dados").
   RUN limparTTs    IN hBoCtblCx.
   RUN SETMostrarMsg IN hBoCtblCx(INPUT FRAME {&frame-name} tgMsg).
   RUN setIntervalEstab IN hBoCtblCx(fiCodEstabIni:SCREEN-VALUE, fiCodEstabFim:SCREEN-VALUE). 
   RUN setDataTransacao IN hBoCtblCx(INPUT FRAME {&FRAME-NAME} fiDtIni,
                        INPUT FRAME {&FRAME-NAME} fiDtFim ).
   RUN setNivel IN hBoCtblCx('fluxo').
   RUN setlistaTpFluxoDescons IN hBoCtblCx('1.02,2.09').
   RUN setCtaCorrenDesconsiderar IN hBoCtblCx('95'). // carterira med - solicitado por Newson em 16/02/2022
   RUN setCtaCorrenDesconsiderar IN hBoCtblCx('mutuo').
   RUN setCtaCorrenDesconsiderar IN hBoCtblCx('555-mut im').
   
   IF fiCtaCorren:SCREEN-VALUE <> '' THEN
      RUN setCtaCorren IN hBoCtblCx(fiCtaCorren:SCREEN-VALUE).

   IF fiNrSeq:SCREEN-VALUE <> '0' THEN
      RUN setSeq IN hBoCtblCx(INT(fiNrSeq:SCREEN-VALUE)).

   RUN buscarRegistros IN hBoCtblCx.
   RUN exportarRegsFluxo IN hBoCtblCx('c:\temp\fluxo_realizado.txt' ).
   run exportarLancsSemCtbl IN hBoCtblCx('c:\temp\fluxo_realizado_sem_ctbl.txt').
   RUN getTTFluxoFechamento IN hBoCtblCx(OUTPUT TABLE ttFluxoFechamento).
   RUN exportarLancsFechamento IN hBoCTblCx('c:\temp\fechamento.txt').


    /*
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
   '04-Previs‰es',
   0,
   '',
   ?,
   '',
   '',
   NO,
   NO  ). 
*/
    RUN pi-finalizar IN h_acomp.
    /***************** exportaá∆o de registros *********************************************/

    OUTPUT TO c:\temp\ParamsFLuxoRealizado.txt.
    ASSIGN nomeEmpresa = IF cbEmpresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '100' THEN 'IMA' ELSE 'MED'.
    /*MESSAGE  fiDtIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}            
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    PUT  'Empresa:'  nomeEmpresa SKIP
          'Data Inicial:' fiDtIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} FORMAT 'x(12)'            
        ' - Data Final:' fiDtFim:SCREEN-VALUE IN FRAME {&FRAME-NAME} FORMAT 'x(12)'            SKIP
        .

    OUTPUT CLOSE.
    

    OS-COMMAND  SILENT value("START excel /t t:\especificos\excel\fluxo_caixa_realizado.xlsx") .
   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNrSeq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNrSeq w-livre
ON LEAVE OF fiNrSeq IN FRAME f-cad /* Sequància */
DO:
  IF INPUT FRAME {&frame-name} fiNrSeq <> 0 AND 
     INPUT FRAME {&frame-name} fiCtaCorren <> '' THEN
     ASSIGN tgMsg:VISIBLE = YES.

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
  DISPLAY fiCodEstabIni fiCodEstabFim fiDtIni fiDtFim fiCtaCorren fiNrSeq tgMsg 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 fiCodEstabIni fiCodEstabFim fiDtIni fiDtFim 
         fiCtaCorren fiNrSeq tgMsg btExecutar 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE limpartabelasTemporarias w-livre 
PROCEDURE limpartabelasTemporarias :
/*------------------------------------------------------------------------------
  limpar todas as tabelas temporarias
------------------------------------------------------------------------------*/




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

  {utp/ut9000.i "ESFL003" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fiDtIni:SCREEN-VALUE = STRING(TODAY,'99/99/9999')
         fiDtFim:SCREEN-VALUE = STRING(TODAY,'99/99/9999')
         tgMsg:VISIBLE = NO.
  run pi-after-initialize.
  RUN esbo/bo_ctbl_cx.p PERSISTE SET hBoCtblCx.
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
IF VALID-HANDLE(hBoCtblCx) THEN
     DELETE PROCEDURE hBoCtblCx . 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retornarDescFluxo w-livre 
PROCEDURE retornarDescFluxo :
/*------------------------------------------------------------------------------
Retorna a descriá∆o do fluxo de caixa PAI do tipo de fluxo passado por parametro
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
Retorna a descriá∆o do fluxo de caixa PAI do tipo de fluxo passado por parametro
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

