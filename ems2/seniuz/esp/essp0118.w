&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadcom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadcom 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0118 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR c-cod-estabel AS CHAR.
DEFINE NEW GLOBAL SHARED VAR i-tipo-saida  AS INT.
DEFINE NEW GLOBAL SHARED VAR g-tipo-mov AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button btn-parametro bt-impr-ticket 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadcom AS WIDGET-HANDLE NO-UNDO.

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
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-cadsim AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01es057 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v02es057 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v03es057 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v04es057 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v05es057 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v06es057 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-impr-ticket 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.17 TOOLTIP "Imprime o Ticket de Pesagem.".

DEFINE BUTTON btn-parametro 
     IMAGE-UP FILE "image/im-param2.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.17 TOOLTIP "Seleá∆o de Veiculos pela situaá∆o de Sa°da.".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btn-parametro AT ROW 1.42 COL 56.57 HELP
          "Paramàtros do Programa"
     bt-impr-ticket AT ROW 1.42 COL 62
     rt-button AT ROW 1.25 COL 1.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadcom ASSIGN
         HIDDEN             = YES
         TITLE              = "Controle de Veiculos na Fabrica"
         HEIGHT             = 17.79
         WIDTH              = 90
         MAX-HEIGHT         = 17.79
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17.79
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadcom 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-cadcom.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadcom
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadcom)
THEN w-cadcom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadcom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadcom w-cadcom
ON END-ERROR OF w-cadcom /* Controle de Veiculos na Fabrica */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
 RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadcom w-cadcom
ON WINDOW-CLOSE OF w-cadcom /* Controle de Veiculos na Fabrica */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-impr-ticket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-impr-ticket w-cadcom
ON CHOOSE OF bt-impr-ticket IN FRAME f-cad /* Button 1 */
DO:
  RUN pi-tipo-mov IN h_v02es057 (OUTPUT g-tipo-mov).
  CASE g-tipo-mov.
    WHEN 1 THEN /* CARGA TECIDO */
        RUN pi-impr-ticket IN h_v03es057.
    WHEN 2 THEN /* DESCARGA ALGOD«O */
        RUN pi-impr-ticket IN h_v05es057.
    WHEN 3 THEN /* DESCARGA OUTROS */ 
        RUN pi-impr-ticket IN h_v03es057.
    WHEN 4 THEN /* CARGA OUTROS */ 
        RUN pi-impr-ticket IN h_v06es057.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-parametro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-parametro w-cadcom
ON CHOOSE OF btn-parametro IN FRAME f-cad /* Button 1 */
DO:
  RUN esdlg/d01es057.w (INPUT-OUTPUT c-cod-estabel,
                        INPUT-OUTPUT i-tipo-saida).
/*  ASSIGN g-tipo-mov = i-tipo-saida. */
  RUN adm-open-query-cases IN h_q01es057.
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-alterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-alterar w-cadcom
ON CHOOSE OF MENU-ITEM mi-alterar /* Alterar */
DO:
  RUN pi-alterar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadcom
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadcom
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-cancelar w-cadcom
ON CHOOSE OF MENU-ITEM mi-cancelar /* Cancelar */
DO:
  RUN pi-cancelar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cadcom
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadcom
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-copiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-copiar w-cadcom
ON CHOOSE OF MENU-ITEM mi-copiar /* Copiar */
DO:
  RUN pi-copiar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-desfazer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-desfazer w-cadcom
ON CHOOSE OF MENU-ITEM mi-desfazer /* Desfazer */
DO:
  RUN pi-desfazer IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-eliminar w-cadcom
ON CHOOSE OF MENU-ITEM mi-eliminar /* Eliminar */
DO:
  RUN pi-eliminar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadcom
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-incluir w-cadcom
ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-incluir IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadcom
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadcom
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadcom
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadcom
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-salvar w-cadcom
ON CHOOSE OF MENU-ITEM mi-salvar /* Salvar */
DO:
  RUN pi-salvar IN h_p-cadsim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadcom
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadcom
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadcom
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadcom 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadcom  _ADM-CREATE-OBJECTS
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

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v02es057.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v02es057 ).
       RUN set-position IN h_v02es057 ( 2.75 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 3.50 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Carga Tecido|Desc. Algod∆o|Desc. Outros|Carga Outros' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 6.33 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 12.42 , 88.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q01es057.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = eszoom/z01es057.w,
                     ProgVaPara = esgo/g01es057.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q01es057 ).
       RUN set-position IN h_q01es057 ( 1.50 , 68.86 ) NO-ERROR.
       /* Size in UIB:  ( 1.00 , 4.00 ) */

       /* Links to SmartPanelCadastro h_p-cadsim. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'State':U , THIS-PROCEDURE ).

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'State':U , h_p-exihel ).

       /* Links to SmartViewer h_v02es057. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'TableIO':U , h_v02es057 ).
       RUN add-link IN adm-broker-hdl ( h_q01es057 , 'Record':U , h_v02es057 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q01es057. */
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01es057 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01es057 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             btn-parametro:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadsim ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             h_p-cadsim , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v02es057 ,
             bt-impr-ticket:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_folder ,
             h_v02es057 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v03es057.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v03es057 ).
       RUN set-position IN h_v03es057 ( 7.50 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.00 , 86.00 ) */

       /* Links to SmartViewer h_v03es057. */
       RUN add-link IN adm-broker-hdl ( h_q01es057 , 'Record':U , h_v03es057 ).
       RUN add-link IN adm-broker-hdl ( h_v02es057 , 'GROUP-ASSIGN':U , h_v03es057 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v03es057 ,
             h_folder , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v05es057.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v05es057 ).
       RUN set-position IN h_v05es057 ( 7.50 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.00 , 86.00 ) */

       /* Links to SmartViewer h_v05es057. */
       RUN add-link IN adm-broker-hdl ( h_q01es057 , 'Record':U , h_v05es057 ).
       RUN add-link IN adm-broker-hdl ( h_v02es057 , 'GROUP-ASSIGN':U , h_v05es057 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v05es057 ,
             h_folder , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v04es057.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v04es057 ).
       RUN set-position IN h_v04es057 ( 7.75 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.75 , 86.00 ) */

       /* Links to SmartViewer h_v04es057. */
       RUN add-link IN adm-broker-hdl ( h_q01es057 , 'Record':U , h_v04es057 ).
       RUN add-link IN adm-broker-hdl ( h_v02es057 , 'GROUP-ASSIGN':U , h_v04es057 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v04es057 ,
             h_folder , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v06es057.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v06es057 ).
       RUN set-position IN h_v06es057 ( 7.50 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.00 , 86.00 ) */

       /* Links to SmartViewer h_v06es057. */
       RUN add-link IN adm-broker-hdl ( h_q01es057 , 'Record':U , h_v06es057 ).
       RUN add-link IN adm-broker-hdl ( h_v02es057 , 'GROUP-ASSIGN':U , h_v06es057 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v06es057 ,
             h_folder , 'AFTER':U ).
    END. /* Page 4 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadcom  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadcom  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadcom)
  THEN DELETE WIDGET w-cadcom.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadcom  _DEFAULT-ENABLE
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
  ENABLE rt-button btn-parametro bt-impr-ticket 
      WITH FRAME f-cad IN WINDOW w-cadcom.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadcom.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadcom 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadcom 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadcom 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  PROPATH=".\especificos\balanca," + PROPATH.

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "ESSP0118" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND FIRST param-estoq NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = param-estoq.estabel-pad
         i-tipo-saida  = 1.
  run pi-after-initialize.
  RUN enable-copia in h_p-cadsim (INPUT NO). /* DESABILITAR O BOT«O COPIA */

  run pi-before-initialize.
  {include/i-inifld.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-campos w-cadcom 
PROCEDURE pi-desabilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-tipo-mov AS INT.

  CASE p-tipo-mov.
    WHEN 1 THEN /* CARGA TECIDO */
        RUN pi-desabilita-campos IN h_v03es057.
    WHEN 2 THEN /* DESCARGA ALGOD«O */
        RUN pi-desabilita-campos IN h_v05es057.
    WHEN 3 THEN /* DESCARGA OUTROS */ 
        RUN pi-desabilita-campos IN h_v04es057.
    WHEN 4 THEN /* CARGA OUTROS */ 
        RUN pi-desabilita-campos IN h_v06es057.

 END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita-folders w-cadcom 
PROCEDURE pi-desabilita-folders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN disable-folder-page IN h_folder (INPUT 1).
RUN disable-folder-page IN h_folder (INPUT 2).
RUN disable-folder-page IN h_folder (INPUT 3).
RUN disable-folder-page IN h_folder (INPUT 4).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-campos w-cadcom 
PROCEDURE pi-habilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-tipo-mov AS INT.
  DEF INPUT PARAMETER p-novo-registro AS LOG.

  CASE p-tipo-mov.
    WHEN 1 THEN /* CARGA TECIDO */
        RUN pi-habilita-campos IN h_v03es057 (INPUT p-novo-registro).
    WHEN 2 THEN /* DESCARGA ALGOD«O */
        RUN pi-habilita-campos IN h_v05es057 (INPUT p-novo-registro).
    WHEN 3 THEN /* DESCARGA OUTROS */ 
        RUN pi-habilita-campos IN h_v04es057 (INPUT p-novo-registro).
    WHEN 4 THEN /* CARGA OUTROS */ 
        RUN pi-habilita-campos IN h_v06es057 (INPUT p-novo-registro).

 END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-folders w-cadcom 
PROCEDURE pi-habilita-folders :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-tipo-mov AS int.

  CASE p-tipo-mov:
      WHEN 1 THEN DO: /* CARGA TECIDO*/
          RUN disable-folder-page IN h_folder (INPUT 2).
          RUN disable-folder-page IN h_folder (INPUT 3).
          RUN disable-folder-page IN h_folder (INPUT 4).
          RUN enable-folder-page IN h_folder (INPUT 1).
          RUN select-page IN THIS-PROCEDURE (1).
      END.
      WHEN 2 THEN DO: /* DESCARGA ALGOD«O */
          RUN disable-folder-page IN h_folder (INPUT 1).
          RUN disable-folder-page IN h_folder (INPUT 2).
          RUN disable-folder-page IN h_folder (INPUT 4).
          RUN enable-folder-page IN h_folder (INPUT 2).
          RUN select-page IN THIS-PROCEDURE (2).
      END.
      WHEN 3 THEN DO: /* DESCARGA OUTROS */
          RUN disable-folder-page IN h_folder (INPUT 1).
          RUN disable-folder-page IN h_folder (INPUT 2).
          RUN disable-folder-page IN h_folder (INPUT 3).
          RUN enable-folder-page IN h_folder (INPUT 3).
          RUN select-page IN THIS-PROCEDURE (3).
      END.
      WHEN 4 THEN DO:  /* CARGA OUTROS */
          RUN disable-folder-page IN h_folder (INPUT 1).
          RUN disable-folder-page IN h_folder (INPUT 3).
          RUN disable-folder-page IN h_folder (INPUT 4).
          RUN enable-folder-page IN h_folder (INPUT 4).
          RUN select-page IN THIS-PROCEDURE (4).
      END.
  END CASE.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita-p-cadsim w-cadcom 
PROCEDURE pi-habilita-p-cadsim :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-acao AS LOG.
  RUN enable-modifica in h_p-cadsim (INPUT p-acao).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-refresh w-cadcom 
PROCEDURE pi-refresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN adm-open-query-cases IN h_q01es057.
  RUN pi-primeiro IN h_p-navega.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadcom  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadcom 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
       Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  RUN pi-tipo-mov IN h_v02es057 (OUTPUT g-tipo-mov).
  IF p-state = 'update-begin' THEN DO:
     RUN pi-desabilita-campos (INPUT g-tipo-mov).
     RUN pi-habilita-campos   (INPUT g-tipo-mov, INPUT NO).
     RUN pi-habilita-folders  (INPUT g-tipo-mov).
  END.
  IF p-state = 'add-record' THEN DO:
     RUN pi-habilita-campos IN h_v02es057.
     RUN pi-desabilita-campos (INPUT g-tipo-mov).
  END.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

