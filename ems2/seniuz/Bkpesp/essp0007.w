&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-concom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-concom 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0007 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VAR num-os-ini   LIKE mov-man.num-os.
DEFINE VAR num-os-fin   LIKE mov-man.num-os.
DEFINE VAR data-abe-ini LIKE mov-man.data-abe.
DEFINE VAR data-abe-fin LIKE mov-man.data-abe.
DEFINE VAR arq-saida    AS CHAR FORMAT "x(45)".

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-3 BUTTON-1 rt-button 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-concom AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V  para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q02es022 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01es022 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v02es022 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v03es022 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v04es022 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v05es022 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 1.13 TOOLTIP "Gerar uma planilha Excel, com os dados do Movimento da Mecƒnica.".

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "image/im-param2.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-param.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.17 TOOLTIP "Parƒmetros da consulta".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     BUTTON-3 AT ROW 1.38 COL 28.43
     BUTTON-1 AT ROW 1.42 COL 33
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 18.


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
  CREATE WINDOW w-concom ASSIGN
         HIDDEN             = YES
         TITLE              = "Movimentos da Manuten‡Æo"
         HEIGHT             = 18
         WIDTH              = 90
         MAX-HEIGHT         = 28.13
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.13
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-concom 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-concom.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-concom
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   L-To-R                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-concom)
THEN w-concom:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-concom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-concom w-concom
ON END-ERROR OF w-concom /* Movimentos da Manuten‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-concom w-concom
ON WINDOW-CLOSE OF w-concom /* Movimentos da Manuten‡Æo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 w-concom
ON CHOOSE OF BUTTON-1 IN FRAME f-cad /* Button 1 */
DO:
  RUN esdlg/d01es022.w (OUTPUT num-os-ini, OUTPUT num-os-fin, 
                        OUTPUT data-abe-ini, OUTPUT data-abe-fin, OUTPUT arq-saida).

  IF arq-saida <> "" THEN DO:
     RUN pi-gera-excel (INPUT num-os-ini, INPUT num-os-fin, 
                        INPUT data-abe-ini, INPUT data-abe-fin, INPUT arq-saida).

     MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
             "Para acess -lo,  abra-o atrav‚s do Excel."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 w-concom
ON CHOOSE OF BUTTON-3 IN FRAME f-cad /* Button 3 */
DO:
  RUN esp\essp0007a.w.
  RUN adm-open-query-cases IN h_q02es022.
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-concom
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-concom
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-concom
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-concom
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-concom
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-concom
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-concom
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-concom
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-concom
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-concom
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-concom
ON CHOOSE OF MENU-ITEM mi-ultimo /* éltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-concom
ON CHOOSE OF MENU-ITEM mi-va-para /* V  para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-concom 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-concom  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 73.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v01es022.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01es022 ).
       RUN set-position IN h_v01es022 ( 2.75 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.75 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Dados Gerais|Sistemas|Atividades|Observa‡äes' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 7.50 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 11.50 , 88.57 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q02es022.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = eszoom\z01es022.w,
                     ProgVaPara = esgo\g01es022.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q02es022 ).
       RUN set-position IN h_q02es022 ( 1.50 , 64.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */

       /* Links to SmartViewer h_v01es022. */
       RUN add-link IN adm-broker-hdl ( h_q02es022 , 'Record':U , h_v01es022 ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Links to SmartQuery h_q02es022. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q02es022 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q02es022 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q02es022 ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v02es022.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v02es022 ).
       RUN set-position IN h_v02es022 ( 8.75 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.00 , 87.00 ) */

       /* Links to SmartViewer h_v02es022. */
       RUN add-link IN adm-broker-hdl ( h_q02es022 , 'Record':U , h_v02es022 ).
       RUN add-link IN adm-broker-hdl ( h_v01es022 , 'GROUP-ASSIGN':U , h_v02es022 ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v03es022.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_v03es022 ).
       RUN set-position IN h_v03es022 ( 8.75 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.00 , 87.00 ) */

       /* Links to SmartViewer h_v03es022. */
       RUN add-link IN adm-broker-hdl ( h_q02es022 , 'Record':U , h_v03es022 ).
       RUN add-link IN adm-broker-hdl ( h_v01es022 , 'GROUP-ASSIGN':U , h_v03es022 ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v04es022.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Initial-Lock = NO-LOCK,
                     Hide-on-Init = no,
                     Disable-on-Init = no,
                     Layout = ,
                     Create-On-Add = ?':U ,
             OUTPUT h_v04es022 ).
       RUN set-position IN h_v04es022 ( 8.75 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.00 , 87.00 ) */

       /* Links to SmartViewer h_v04es022. */
       RUN add-link IN adm-broker-hdl ( h_q02es022 , 'Record':U , h_v04es022 ).
       RUN add-link IN adm-broker-hdl ( h_v01es022 , 'GROUP-ASSIGN':U , h_v04es022 ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v05es022.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v05es022 ).
       RUN set-position IN h_v05es022 ( 8.75 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 10.00 , 87.00 ) */

       /* Links to SmartViewer h_v05es022. */
       RUN add-link IN adm-broker-hdl ( h_q02es022 , 'Record':U , h_v05es022 ).
       RUN add-link IN adm-broker-hdl ( h_v01es022 , 'GROUP-ASSIGN':U , h_v05es022 ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 4 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-concom  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-concom  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-concom)
  THEN DELETE WIDGET w-concom.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-concom  _DEFAULT-ENABLE
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
  ENABLE BUTTON-3 BUTTON-1 rt-button 
      WITH FRAME f-cad IN WINDOW w-concom.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-concom.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-concom 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-concom 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-concom 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

 {utp/ut9000.i "ESSP0007" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel w-concom 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF INPUT PARAMETER p-arquivo AS CHAR.

   def var h-prog as handle no-undo.
   run utp/ut-utils.p persistent set h-prog.

   run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

   delete procedure h-prog.
   PAUSE 5 NO-MESSAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel w-concom 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER p-num-os-ini   LIKE mov-man.num-os.
    DEF INPUT PARAMETER p-num-os-fin   LIKE mov-man.num-os.
    DEF INPUT PARAMETER p-data-abe-ini LIKE mov-man.data-abe.
    DEF INPUT PARAMETER p-data-abe-fin LIKE mov-man.data-abe.
    DEF INPUT PARAMETER p-arq-saida    AS CHAR FORMAT "x(45)".
    
    DEF VAR c-empresa LIKE empresa.razao-social.
    
    DEFINE VAR i-canal AS INTEGER.
    DEFINE VAR sys     AS INTEGER.
    DEFINE VAR i-Lin   AS INTEGER.
    DEFINE VAR c-lin   AS CHARACTER FORMAT "x(500)".
    
    /*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
    DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
    ENABLE ALL WITH FRAME frm_excel.
    
    /* bloco principal do programa */
    find first param-global no-lock no-error.
    find first empresa
         where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 
    
    assign c-empresa = (if avail empresa then empresa.razao-social else "").
    
    RUN pi-abre-excel (INPUT "").
    PAUSE 3 NO-MESSAGE.
    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".
    
    ASSIGN c-Lin = c-empresa + " - " + " TABELA DE MOVIMENTOS DA MEC¶NICA - DATA: " + STRING(TODAY) +
                                       " - " + STRING(TIME,"HH:MM").
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    
    DDE SEND i-canal SOURCE "NéMERO-OS"   ITEM "L3C1". 
    DDE SEND i-canal SOURCE "SETOR"       ITEM "L3C2".
    DDE SEND i-canal SOURCE "DESCRI€ÇO"   ITEM "L3C3".
    DDE SEND i-canal SOURCE "DATA-ABE"    ITEM "L3C4".
    DDE SEND i-canal SOURCE "HORA-ABE"    ITEM "L3C5".
    DDE SEND i-canal SOURCE "COD-MAQ"     ITEM "L3C6".
    DDE SEND i-canal SOURCE "DESCRI€ÇO"   ITEM "L3C7".
    DDE SEND i-canal SOURCE "FUNCIONµRIO" ITEM "L3C8".
    DDE SEND i-canal SOURCE "HORA-PICO"   ITEM "L3C9".
    DDE SEND i-canal SOURCE "SIST-CONST"  ITEM "L3C10".
    DDE SEND i-canal SOURCE "OBSERVA€ÇO"  ITEM "L3C11".
    DDE SEND i-canal SOURCE "COD-µREA"    ITEM "L3C12".
    DDE SEND i-canal SOURCE "DESCRI€ÇO"   ITEM "L3C13".
    DDE SEND i-canal SOURCE "TIPO-MAN"    ITEM "L3C14".
    DDE SEND i-canal SOURCE "HORA-RECEB"  ITEM "L3C15".
    DDE SEND i-canal SOURCE "SIST-EXEC"   ITEM "L3C16".
    DDE SEND i-canal SOURCE "SUB-SIST"    ITEM "L3C17".
    DDE SEND i-canal SOURCE "DESCRI€ÇO"   ITEM "L3C18".

    DDE SEND i-canal SOURCE "EXECU€ÇO"    ITEM "L3C19".
    DDE SEND i-canal SOURCE "ATIV"        ITEM "L3C20".
    DDE SEND i-canal SOURCE "FUNC.EXEC"   ITEM "L3C21".
    DDE SEND i-canal SOURCE "HORA-INIC"   ITEM "L3C22".
    DDE SEND i-canal SOURCE "HORA-TERM"   ITEM "L3C23".
    DDE SEND i-canal SOURCE "MIN-PARADO"  ITEM "L3C24".
    DDE SEND i-canal SOURCE "OS-VISADA"   ITEM "L3C25".

    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.29)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(5.86)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(9.71)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(9.00)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.57)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(25.43)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(12.14)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.00)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.86)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(106.57)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C12~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(9.29)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C13~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(17.71)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C14~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.57)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C15~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(11.29)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C16~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(9.43)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C17~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.43)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C18~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(27.57)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C19~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(27.57)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C20~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(4.29)]". 

    DDE EXECUTE i-canal COMMAND "[select(~"C21~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(9.71)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C22~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(9.57)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C23~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.14)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C24~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(11.14)]". 
   
    DDE EXECUTE i-canal COMMAND "[select(~"C25~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.00)]". 

    DDE EXECUTE i-canal COMMAND '[select(~"C1:C25~")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys     COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
    
    ASSIGN i-Lin = 4.
    
    FOR EACH mov-man WHERE mov-man.num-os   >= p-num-os-ini
                       AND mov-man.num-os   <= p-num-os-fin
                       AND mov-man.data-abe >= p-data-abe-ini
                       AND mov-man.data-abe <= p-data-abe-fin
                     NO-LOCK:
        FIND set-mec WHERE set-mec.codigo = mov-man.cod-setor NO-LOCK NO-ERROR.
        FIND maq-mec WHERE maq-mec.codigo = mov-man.cod-maq NO-LOCK NO-ERROR.
        FIND area-mec WHERE area-mec.codigo = mov-man.cod-area NO-LOCK NO-ERROR.
        FIND ssist-mec WHERE ssist-mec.codigo = mov-man.ssist-exec[1] NO-LOCK NO-ERROR.

        DDE SEND i-canal SOURCE mov-man.num-os      ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
        DDE SEND i-canal SOURCE mov-man.cod-setor   ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
        IF AVAIL set-mec THEN
           DDE SEND i-canal SOURCE set-mec.descricao   ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        DDE SEND i-canal SOURCE STRING(mov-man.data-abe) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(mov-man.hora-abe,"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        DDE SEND i-canal SOURCE mov-man.cod-maq     ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        IF AVAIL maq-mec THEN
           DDE SEND i-canal SOURCE maq-mec.descricao   ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        DDE SEND i-canal SOURCE STRING(mov-man.func-abe)  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
        DDE SEND i-canal SOURCE STRING(mov-man.hora-pico) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
        DDE SEND i-canal SOURCE mov-man.sist-const  ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
        DDE SEND i-canal SOURCE mov-man.observ      ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
        DDE SEND i-canal SOURCE mov-man.cod-area    ITEM "L" + TRIM(STRING(i-Lin)) + "C12".
        IF AVAIL area-mec THEN
           DDE SEND i-canal SOURCE area-mec.descricao  ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
        DDE SEND i-canal SOURCE mov-man.tipo-man    ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
        DDE SEND i-canal SOURCE mov-man.hor-receb   ITEM "L" + TRIM(STRING(i-Lin)) + "C15".
        DDE SEND i-canal SOURCE mov-man.sist-exec[1]  ITEM "L" + TRIM(STRING(i-Lin)) + "C16".
        DDE SEND i-canal SOURCE mov-man.ssist-exec[1] ITEM "L" + TRIM(STRING(i-Lin)) + "C17".
        IF AVAIL ssist-mec THEN
           DDE SEND i-canal SOURCE ssist-mec.descricao   ITEM "L" + TRIM(STRING(i-Lin)) + "C18".
        DDE SEND i-canal SOURCE mov-man.desc-exec[1]     ITEM "L" + TRIM(STRING(i-Lin)) + "C19".
        DDE SEND i-canal SOURCE mov-man.ativ-exec[1]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
        DDE SEND i-canal SOURCE STRING(mov-man.func-exec[1]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
        DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[1],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
        DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[1],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
        DDE SEND i-canal SOURCE STRING(mov-man.min-parado[1]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
        DDE SEND i-canal SOURCE STRING(mov-man.os-visada) ITEM "L" + TRIM(STRING(i-Lin)) + "C25".
        ASSIGN i-Lin = i-Lin + 1.
        
        FIND ssist-mec WHERE ssist-mec.codigo = mov-man.ssist-exec[2] NO-LOCK NO-ERROR.
        IF AVAIL ssist-mec THEN DO:
           DDE SEND i-canal SOURCE ssist-mec.descricao ITEM "L" + TRIM(STRING(i-Lin)) + "C18".
           DDE SEND i-canal SOURCE mov-man.desc-exec[2]ITEM "L" + TRIM(STRING(i-Lin)) + "C19".
        END.
        IF mov-man.ativ-exec[2] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[2]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[2]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[2],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[2],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[2]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
        END.
        IF AVAIL ssist-mec OR mov-man.ativ-exec[2] <> "" THEN
           ASSIGN i-Lin = i-Lin + 1.

        FIND ssist-mec WHERE ssist-mec.codigo = mov-man.ssist-exec[3] NO-LOCK NO-ERROR.
        IF AVAIL ssist-mec THEN DO:
           DDE SEND i-canal SOURCE ssist-mec.descricao ITEM "L" + TRIM(STRING(i-Lin)) + "C18".
           DDE SEND i-canal SOURCE mov-man.desc-exec[3]ITEM "L" + TRIM(STRING(i-Lin)) + "C19".
        END.
        IF mov-man.ativ-exec[3] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[3]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[3]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[3],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[3],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[3]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
        END.
        IF AVAIL ssist-mec OR mov-man.ativ-exec[3] <> "" THEN
           ASSIGN i-Lin = i-Lin + 1.

        FIND ssist-mec WHERE ssist-mec.codigo = mov-man.ssist-exec[4] NO-LOCK NO-ERROR.
        IF AVAIL ssist-mec THEN DO:
           DDE SEND i-canal SOURCE ssist-mec.descricao ITEM "L" + TRIM(STRING(i-Lin)) + "C18".
           DDE SEND i-canal SOURCE mov-man.desc-exec[4]ITEM "L" + TRIM(STRING(i-Lin)) + "C19".
        END.
        IF mov-man.ativ-exec[4] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[4]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[4]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[4],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[4],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[4]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
        END.
        IF AVAIL ssist-mec OR mov-man.ativ-exec[4] <> "" THEN
           ASSIGN i-Lin = i-Lin + 1.

        FIND ssist-mec WHERE ssist-mec.codigo = mov-man.ssist-exec[5] NO-LOCK NO-ERROR.
        IF AVAIL ssist-mec THEN DO:
           DDE SEND i-canal SOURCE ssist-mec.descricao ITEM "L" + TRIM(STRING(i-Lin)) + "C18".
           DDE SEND i-canal SOURCE mov-man.desc-exec[5]ITEM "L" + TRIM(STRING(i-Lin)) + "C19".
        END.
        IF mov-man.ativ-exec[5] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[5]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[5]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[5],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[5],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[5]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
        END.
        IF AVAIL ssist-mec OR mov-man.ativ-exec[5] <> "" THEN
           ASSIGN i-Lin = i-Lin + 1.

        IF mov-man.ativ-exec[6] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[6]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[6]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[6],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[6],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[6]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
           ASSIGN i-Lin = i-Lin + 1.
        END.
        IF mov-man.ativ-exec[7] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[7]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[7]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[7],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[7],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[7]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
           ASSIGN i-Lin = i-Lin + 1.
        END.
        IF mov-man.ativ-exec[8] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[8]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[8]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[8],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[8],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[8]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
           ASSIGN i-Lin = i-Lin + 1.
        END.
        IF mov-man.ativ-exec[9] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[9]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[9]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[9],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[9],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[9]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
           ASSIGN i-Lin = i-Lin + 1.
        END.
        IF mov-man.ativ-exec[10] <> "" THEN DO:
           DDE SEND i-canal SOURCE mov-man.ativ-exec[10]  ITEM "L" + TRIM(STRING(i-Lin)) + "C20".
           DDE SEND i-canal SOURCE STRING(mov-man.func-exec[10]) ITEM "L" + TRIM(STRING(i-Lin)) + "C21".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-iexec[10],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C22".
           DDE SEND i-canal SOURCE STRING(mov-man.hora-texec[10],"99:99") ITEM "L" + TRIM(STRING(i-Lin)) + "C23".
           DDE SEND i-canal SOURCE STRING(mov-man.min-parado[10]) ITEM "L" + TRIM(STRING(i-Lin)) + "C24".
           ASSIGN i-Lin = i-Lin + 1.
        END.
    END.
    
    OS-DELETE VALUE(p-arq-saida).
    DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    DDE EXECUTE   sys COMMAND "[close(0)]". 
    DDE EXECUTE   sys COMMAND "[quit()]". 
    DDE TERMINATE sys.
    
    HIDE FRAME frm_excel.
    CLEAR FRAME frm_excel.
    DISABLE ALL WITH FRAME frm_excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-concom  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-concom 
PROCEDURE state-changed :
/* -----------------------------------------------------------
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

