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

DEFINE VARIABLE iFornecIni AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE iFornecFim AS INTEGER     NO-UNDO INIT 9999999.
DEFINE VARIABLE cTitIni    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTitFim    AS CHARACTER   NO-UNDO INIT "zzzzzzzzz".
DEFINE BUFFER bfTit   FOR val_tit_ap.
DEFINE BUFFER bfMovto FOR rat_movto_tit_ap .
DEFINE VARIABLE deOutro         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE deOutroSaldo    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE deOutroPagto    AS DECIMAL     NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rt-button fiDtIni fiDtFim fiFornecedor ~
fiTitulo fiTpFluxoAtu fiTpfluxoNovo btAlterar 
&Scoped-Define DISPLAYED-OBJECTS fiDtIni fiDtFim fiFornecedor fiTitulo ~
fiTpFluxoAtu fiTpfluxoNovo 

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
     LABEL "Alterar Tp.Fluxo" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiDtFim AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDtIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Ini" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiFornecedor AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTitulo AS CHARACTER FORMAT "X(25)":U 
     LABEL "T¡tulo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTpFluxoAtu AS CHARACTER FORMAT "X(25)":U 
     LABEL "Tp.Fluxo Atual" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTpfluxoNovo AS CHARACTER FORMAT "X(25)":U 
     LABEL "Novo Tp.Fluxo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiDtIni AT ROW 3.25 COL 13.86 COLON-ALIGNED WIDGET-ID 2
     fiDtFim AT ROW 3.25 COL 46.14 COLON-ALIGNED WIDGET-ID 4
     fiFornecedor AT ROW 4.42 COL 14 COLON-ALIGNED WIDGET-ID 12
     fiTitulo AT ROW 4.5 COL 46.14 COLON-ALIGNED WIDGET-ID 14
     fiTpFluxoAtu AT ROW 5.63 COL 13.86 COLON-ALIGNED WIDGET-ID 6
     fiTpfluxoNovo AT ROW 5.71 COL 46 COLON-ALIGNED WIDGET-ID 8
     btAlterar AT ROW 7.21 COL 15.29 WIDGET-ID 10
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
         TITLE              = "Alterar Titulo de Fluxo APB"
         HEIGHT             = 14.13
         WIDTH              = 90
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Alterar Titulo de Fluxo APB */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Alterar Titulo de Fluxo APB */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAlterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAlterar w-livre
ON CHOOSE OF btAlterar IN FRAME f-cad /* Alterar Tp.Fluxo */
DO:
  DISABLE TRIGGERS FOR LOAD OF  tit_ap .
  DISABLE TRIGGERS FOR LOAD OF  val_tit_ap .
  DISABLE TRIGGERS FOR LOAD OF rat_movto_tit_ap.

  IF INPUT FRAME {&FRAME-NAME} fiFornecedor <> 0 THEN
     ASSIGN iFornecIni = INPUT FRAME {&FRAME-NAME} fiFornecedor
            iFornecFim = INPUT FRAME {&FRAME-NAME} fiFornecedor .
  ELSE 
     ASSIGN iFornecIni = 0
            iFornecFim = 99999999 .

  IF INPUT FRAME {&FRAME-NAME} fiTitulo <> '' THEN
     ASSIGN cTitIni = INPUT FRAME {&FRAME-NAME} fiTitulo
            cTitFim = INPUT FRAME {&FRAME-NAME} fiTitulo .
  ELSE 
     ASSIGN cTitIni = ''
            cTitFim = 'zzzzzzzzzz' .



  ASSIGN  btAlterar:SENSITIVE = FALSE
          btAlterar:LABEL = "Processando....".
  FOR EACH tit_ap NO-LOCK
      WHERE tit_ap.dat_emis_docto >= INPUT FRAME {&FRAME-NAME} fidtIni
      AND   tit_ap.dat_emis_docto <= INPUT FRAME {&FRAME-NAME} fidtFim
      AND tit_ap.cdn_fornec >= iFornecIni
      AND   tit_ap.cdn_fornec <= iFornecFIm
      AND   tit_ap.cod_tit_ap >= cTitIni
      AND   tit_ap.cod_tit_ap <= cTitFim .
      ASSIGN deOutro = 0.
      FIND val_tit_ap OF  tit_ap
           WHERE val_tit_ap.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoNovo
          EXCLUSIVE-LOCK NO-ERROR.
      FIND bfTit OF  tit_ap
           WHERE bfTit.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoAtu
          EXCLUSIVE-LOCK NO-ERROR.


      IF AVAIL  val_tit_ap AND AVAIL bfTit THEN DO:
          ASSIGN deOutro        = val_tit_ap.val_origin_tit_ap 
                 deOutroSaldo   = val_tit_ap.val_sdo_tit_ap 
                 deOutroPagto   = val_tit_ap.val_pagto_tit_ap .
          DELETE val_tit_ap.
          /*MESSAGE 'achei o titulo com o tipo de fluxo:' 
              INPUT FRAME {&FRAME-NAME} fiTpFluxoNovo 'no valor'  deOutro ' e deletei'
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

       END.    
       FOR EACH val_tit_ap OF tit_ap.
          /*MESSAGE val_tit_ap.cod_tip_fluxo
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
          IF val_tit_ap.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoAtu THEN
             ASSIGN val_tit_ap.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoNovo 
                    val_tit_ap.val_origin_tit_ap = val_tit_ap.val_origin_tit_ap + deOutro
                    val_tit_ap.val_sdo_tit_ap    = val_tit_ap.val_sdo_tit_ap    + deOutroSaldo
                    val_tit_ap.val_pagto_tit_ap  = val_tit_ap.val_pagto_tit_ap  + deOutroPagto .

      END.
      
      FOR EACH movto_tit_ap NO-LOCK  OF tit_ap.
          ASSIGN deOutro = 0.
          FIND FIRST rat_movto_tit_ap OF movto_tit_ap
               WHERE rat_movto_tit_ap.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoNovo
               EXCLUSIVE-LOCK NO-ERROR.
          FIND FIRST bfMovto OF movto_tit_ap
               WHERE bfMovto.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoAtu
               EXCLUSIVE-LOCK NO-ERROR.

          IF AVAIL rat_movto_tit_ap AND AVAIL bfMovto THEN DO:
             ASSIGN deOutro = rat_movto_tit_ap.val_aprop_ctbl .
             DELETE rat_movto_tit_ap.
             /* MESSAGE 'vl. outro:' deOutro
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
          END.
          FOR EACH rat_movto_tit_ap OF movto_tit_ap.
               /*MESSAGE rat_movto_tit_ap.val_aprop_ctbl '+'  deOutro
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
               IF rat_movto_tit_ap.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoAtu THEN
                 ASSIGN rat_movto_tit_ap.cod_tip_fluxo = INPUT FRAME {&FRAME-NAME} fiTpFluxoNovo 
                        rat_movto_tit_ap.val_aprop_ctbl = rat_movto_tit_ap.val_aprop_ctbl + deOutro.
          END.
      END.

  END.
  ASSIGN  btAlterar:SENSITIVE = TRUE
          btAlterar:LABEL = "Alterar Tp.Fluxo".
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
  DISPLAY fiDtIni fiDtFim fiFornecedor fiTitulo fiTpFluxoAtu fiTpfluxoNovo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fiDtIni fiDtFim fiFornecedor fiTitulo fiTpFluxoAtu 
         fiTpfluxoNovo btAlterar 
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

