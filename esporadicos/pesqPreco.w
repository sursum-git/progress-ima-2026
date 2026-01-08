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
{include/i-prgvrs.i pesqpreco 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{esbo/boPrecosItemRef.i}
{esbo\boMsg.i}

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
&Scoped-Define ENABLED-OBJECTS rt-button RECT-1 cbTb cbTipoPreco codRefer ~
itCodigo nrContainer prazoMedio dtRefer btExec vlPreco idPrecoTabela ~
vlPrecoOutlet idPrecoOutlet vlPrecoDolar idPrecoTabelaDolar 
&Scoped-Define DISPLAYED-OBJECTS cbTb cbTipoPreco codRefer itCodigo ~
nrContainer prazoMedio dtRefer vlPreco idPrecoTabela vlPrecoOutlet ~
idPrecoOutlet vlPrecoDolar idPrecoTabelaDolar 

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
DEFINE BUTTON btExec 
     LABEL "Buscar Preáo" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cbTb AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Tabela" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Padr∆o",1,
                     "Rubi",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbTipoPreco AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Tipo Preáo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "PE",1,
                     "PI",2
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE codRefer AS CHARACTER FORMAT "X(5)":U 
     LABEL "Refer." 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE dtRefer AS DATE FORMAT "99/99/9999":U INITIAL ? 
     LABEL "Dt.Refer." 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE idPrecoOutlet AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "ID Outlet" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .79 NO-UNDO.

DEFINE VARIABLE idPrecoTabela AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "ID Tabela" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .79 NO-UNDO.

DEFINE VARIABLE idPrecoTabelaDolar AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "ID Tabela" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .79 NO-UNDO.

DEFINE VARIABLE itCodigo AS CHARACTER FORMAT "X(20)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE nrContainer AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE prazoMedio AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Prazo MÇdio" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE vlPreco AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Preáo Tabela" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE vlPrecoDolar AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Preáo US$ Tabela" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE vlPrecoOutlet AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Preáo Outlet" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 6.13.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     cbTb AT ROW 3.25 COL 13 COLON-ALIGNED WIDGET-ID 6
     cbTipoPreco AT ROW 3.25 COL 40 COLON-ALIGNED WIDGET-ID 8
     codRefer AT ROW 4.63 COL 39.86 COLON-ALIGNED WIDGET-ID 4
     itCodigo AT ROW 4.67 COL 13 COLON-ALIGNED WIDGET-ID 2
     nrContainer AT ROW 6.04 COL 13 COLON-ALIGNED WIDGET-ID 12
     prazoMedio AT ROW 6.04 COL 40 COLON-ALIGNED WIDGET-ID 22
     dtRefer AT ROW 7.25 COL 13 COLON-ALIGNED WIDGET-ID 40
     btExec AT ROW 7.25 COL 41.43 WIDGET-ID 10
     vlPreco AT ROW 9 COL 16.72 COLON-ALIGNED WIDGET-ID 18
     idPrecoTabela AT ROW 9 COL 43.57 COLON-ALIGNED WIDGET-ID 20
     vlPrecoOutlet AT ROW 10.08 COL 16.72 COLON-ALIGNED WIDGET-ID 26
     idPrecoOutlet AT ROW 10.08 COL 43.57 COLON-ALIGNED WIDGET-ID 24
     vlPrecoDolar AT ROW 11.46 COL 16.86 COLON-ALIGNED WIDGET-ID 38
     idPrecoTabelaDolar AT ROW 11.46 COL 43.57 COLON-ALIGNED WIDGET-ID 36
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.88 COL 2 WIDGET-ID 16
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
         HEIGHT             = 14.13
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
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
ASSIGN 
       idPrecoOutlet:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       idPrecoTabela:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       idPrecoTabelaDolar:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       vlPreco:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       vlPrecoDolar:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       vlPrecoOutlet:READ-ONLY IN FRAME f-cad        = TRUE.

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


&Scoped-define SELF-NAME btExec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExec w-livre
ON CHOOSE OF btExec IN FRAME f-cad /* Buscar Preáo */
DO:
    DEFINE VARIABLE h-bo    AS HANDLE       NO-UNDO.
    DEFINE VARIABLE vlReal  AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE vlDolar AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE idPreco AS INTEGER     NO-UNDO.
    DEFINE VARIABLE tipoRetorno AS CHARACTER   NO-UNDO.
    
    
    RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo.
    RUN iniciarBos      IN h-bo.
    RUN limparTTPreco   IN h-bo.
    RUN limparTTMsg     IN h-bo.
    RUN setTbPreco      IN h-bo(cbTb:SCREEN-VALUE). //1-padrao   2-rubi
    RUN setItem         IN h-bo(itCodigo:SCREEN-VALUE). 
    RUN setRef          IN h-bo(codRefer:SCREEN-VALUE). 
    RUN setNrContainer  IN h-bo(nrContainer:SCREEN-VALUE).
    RUN setTipoBusca    IN h-bo(IF nrContainer:SCREEN-VALUE <> '0' THEN 2 ELSE 1 ). // 0- todos, 1- pe, 2- pi
    RUN setPrazoMedio   IN h-bo(prazoMedio:SCREEN-VALUE).
    RUN setDtRefer      IN h-bo(INPUT FRAME {&frame-name} dtRefer).
    //RUN setUfCliente    IN h-bo(). como n∆o tem mais IMA n∆o precisa
    RUN buscarPrecos    IN h-bo.

    IF nrContainer:SCREEN-VALUE = '0' THEN DO:
      IF cbTb:SCREEN-VALUE = '1' THEN DO:
         RUN getPrecoPrazo   IN h-bo (INPUT 'outlet',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco).
         ASSIGN vlPrecoOutlet:SCREEN-VALUE =  STRING(vlReal)
                idPrecoOutlet:SCREEN-VALUE = STRING(idPreco).

      END.
      RUN getPrecoPrazo   IN h-bo (INPUT 'pe',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco).
      ASSIGN vlPreco:SCREEN-VALUE =  STRING(vlReal)
             idPrecoTabela:SCREEN-VALUE = STRING(idPreco)
             vlPrecoDolar:SCREEN-VALUE = '0' 
             idPrecoTabelaDolar:SCREEN-VALUE = '0'.

    END.
    ELSE DO:
      RUN getPrecoPrazo   IN h-bo (INPUT 'pi',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco ).  

      ASSIGN vlPrecoDolar:SCREEN-VALUE =  STRING(vlDolar)
             idPrecoTabelaDolar:SCREEN-VALUE = STRING(idPreco)
             vlPreco:SCREEN-VALUE = STRING(vlReal) 
             idPrecoTabela:SCREEN-VALUE = STRING(idPreco)
             vlPrecoOutlet:SCREEN-VALUE = '0'
             idPrecoOutlet:SCREEN-VALUE = '0'.
    END.
    

    RUN finalizarBos IN h-bo.
    IF VALID-HANDLE(h-bo) THEN
       DELETE PROCEDURE h-bo.





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
             cbTb:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY cbTb cbTipoPreco codRefer itCodigo nrContainer prazoMedio dtRefer 
          vlPreco idPrecoTabela vlPrecoOutlet idPrecoOutlet vlPrecoDolar 
          idPrecoTabelaDolar 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-1 cbTb cbTipoPreco codRefer itCodigo nrContainer 
         prazoMedio dtRefer btExec vlPreco idPrecoTabela vlPrecoOutlet 
         idPrecoOutlet vlPrecoDolar idPrecoTabelaDolar 
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

  {utp/ut9000.i "pesqpreco" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 ASSIGN dtrefer:SCREEN-VALUE  = STRING(TODAY). 
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

