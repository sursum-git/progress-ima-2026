&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsim 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-cadsim
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tg-item tg-refer tg-cliente tg-dt-emissao ~
tg-repres tg-regiao rs-selecao RECT-2 RECT-3 rt-button 
&Scoped-Define DISPLAYED-OBJECTS tg-item tg-refer tg-cliente tg-dt-emissao ~
tg-repres tg-regiao rs-selecao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&Èltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V† para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-incluir     LABEL "&Incluir"       ACCELERATOR "CTRL-INS"
       MENU-ITEM mi-copiar      LABEL "C&opiar"        ACCELERATOR "CTRL-C"
       MENU-ITEM mi-alterar     LABEL "A&lterar"       ACCELERATOR "CTRL-A"
       MENU-ITEM mi-eliminar    LABEL "&Eliminar"      ACCELERATOR "CTRL-DEL"
       RULE
       MENU-ITEM mi-desfazer    LABEL "&Desfazer"      ACCELERATOR "CTRL-U"
       MENU-ITEM mi-cancelar    LABEL "&Cancelar"      ACCELERATOR "CTRL-F4"
       RULE
       MENU-ITEM mi-salvar      LABEL "Sal&var"        ACCELERATOR "CTRL-S"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU mi-ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  mi-ajuda       LABEL "A&juda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-cadsim AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rs-selecao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Item", 1,
"Referencia", 2,
"Cliente", 3,
"Data Emissao", 4,
"Representante", 5,
"Regiao", 6
     SIZE 20 BY 6.75 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 3.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 7.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tg-cliente AS LOGICAL INITIAL yes 
     LABEL "Cliente" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-dt-emissao AS LOGICAL INITIAL yes 
     LABEL "Data daEmiss∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg-item AS LOGICAL INITIAL yes 
     LABEL "Item" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-refer AS LOGICAL INITIAL yes 
     LABEL "Referencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .83 NO-UNDO.

DEFINE VARIABLE tg-regiao AS LOGICAL INITIAL yes 
     LABEL "Regi∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-repres AS LOGICAL INITIAL yes 
     LABEL "Representante" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     tg-item AT ROW 4 COL 13.57
     tg-refer AT ROW 4 COL 40
     tg-cliente AT ROW 4 COL 68
     tg-dt-emissao AT ROW 5 COL 13.57
     tg-repres AT ROW 5 COL 40
     tg-regiao AT ROW 5 COL 68
     rs-selecao AT ROW 8.83 COL 31.57 NO-LABEL
     RECT-2 AT ROW 3.5 COL 9
     RECT-3 AT ROW 8.33 COL 27
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-cadsim
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsim ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o <Insira o complemento>"
         HEIGHT             = 17
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
         FONT               = 1
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-cadsim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   L-To-R                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Manutená∆o <Insira o complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Manutená∆o <Insira o complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-alterar w-cadsim
ON CHOOSE OF MENU-ITEM mi-alterar /* Alterar */
DO:
  RUN pi-alterar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadsim
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadsim
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-cancelar w-cadsim
ON CHOOSE OF MENU-ITEM mi-cancelar /* Cancelar */
DO:
  RUN pi-cancelar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cadsim
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadsim
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-copiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-copiar w-cadsim
ON CHOOSE OF MENU-ITEM mi-copiar /* Copiar */
DO:
  RUN pi-copiar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-desfazer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-desfazer w-cadsim
ON CHOOSE OF MENU-ITEM mi-desfazer /* Desfazer */
DO:
  RUN pi-desfazer IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-eliminar w-cadsim
ON CHOOSE OF MENU-ITEM mi-eliminar /* Eliminar */
DO:
  RUN pi-eliminar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadsim
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-incluir w-cadsim
ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-incluir IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadsim
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadsim
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadsim
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadsim
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-salvar w-cadsim
ON CHOOSE OF MENU-ITEM mi-salvar /* Salvar */
DO:
  RUN pi-salvar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadsim
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadsim
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-cliente w-cadsim
ON VALUE-CHANGED OF tg-cliente IN FRAME f-cad /* Cliente */
DO:
    IF INPUT FRAME {&FRAME-NAME} tg-cliente = YES THEN 
       rs-selecao:ENABLE(ENTRY(5,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
    ELSE
       rs-selecao:DISABLE(ENTRY(5,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-dt-emissao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-dt-emissao w-cadsim
ON VALUE-CHANGED OF tg-dt-emissao IN FRAME f-cad /* Data daEmiss∆o */
DO:
    IF INPUT FRAME {&FRAME-NAME} tg-dt-emissao = YES THEN 
       rs-selecao:ENABLE(ENTRY(7,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
    ELSE
       rs-selecao:DISABLE(ENTRY(7,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-item w-cadsim
ON VALUE-CHANGED OF tg-item IN FRAME f-cad /* Item */
DO:
    IF INPUT FRAME {&FRAME-NAME} tg-item = YES THEN 
       rs-selecao:ENABLE(ENTRY(1,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
    ELSE
       rs-selecao:DISABLE(ENTRY(1,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-refer w-cadsim
ON VALUE-CHANGED OF tg-refer IN FRAME f-cad /* Referencia */
DO:
    IF INPUT FRAME {&FRAME-NAME} tg-refer = YES THEN 
       rs-selecao:ENABLE(ENTRY(3,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
    ELSE
       rs-selecao:DISABLE(ENTRY(3,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-regiao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-regiao w-cadsim
ON VALUE-CHANGED OF tg-regiao IN FRAME f-cad /* Regi∆o */
DO:
    IF INPUT FRAME {&FRAME-NAME} tg-regiao = YES THEN 
      rs-selecao:ENABLE(ENTRY(11,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
   ELSE
      rs-selecao:DISABLE(ENTRY(11,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-repres w-cadsim
ON VALUE-CHANGED OF tg-repres IN FRAME f-cad /* Representante */
DO:
    IF INPUT FRAME {&FRAME-NAME} tg-repres = YES THEN 
       rs-selecao:ENABLE(ENTRY(9,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
    ELSE
       rs-selecao:DISABLE(ENTRY(9,(rs-selecao:RADIO-BUTTONS IN FRAME {&FRAME-NAME}))).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsim 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsim  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-cadsim.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cadsim ).
       RUN set-position IN h_p-cadsim ( 1.33 , 27.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 28.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 73.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'State':U , h_p-exihel ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             tg-item:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadsim ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             h_p-cadsim , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadsim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
  THEN DELETE WIDGET w-cadsim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsim  _DEFAULT-ENABLE
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
  DISPLAY tg-item tg-refer tg-cliente tg-dt-emissao tg-repres tg-regiao 
          rs-selecao 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  ENABLE tg-item tg-refer tg-cliente tg-dt-emissao tg-repres tg-regiao 
         rs-selecao RECT-2 RECT-3 rt-button 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
 
  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadsim  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-cadsim, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadsim 
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

