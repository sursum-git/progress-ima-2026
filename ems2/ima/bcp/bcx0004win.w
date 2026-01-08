&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-cadsi4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsi4 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i BCX0004WIN 1.00.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE STREAM s-bat.
DEFINE STREAM s-coletor.
DEFINE STREAM e-coletor.

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD i-sequen AS INT             
    FIELD cd-erro  AS INT
    FIELD mensagem AS CHAR FORMAT "x(255)".
  
PROCEDURE FindWindowA EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intClassName AS LONG.
    DEFINE INPUT  PARAMETER chrCaption   AS CHARACTER.
    DEFINE RETURN PARAMETER intHandle    AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-cadsi4
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-etq-arquivo bt-diversas ~
bt-imprime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsi4 AS WIDGET-HANDLE NO-UNDO.

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
DEFINE VARIABLE h_p-cadsi2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01im003 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v03im001 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-diversas 
     IMAGE-UP FILE "image/im-f-dc.bmp":U
     LABEL "Button 1" 
     SIZE 6 BY 1.25 TOOLTIP "Imprime Etiquetas pelo Coletor".

DEFINE BUTTON bt-etq-arquivo 
     IMAGE-UP FILE "image/gr-rel.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-carga.bmp":U
     LABEL "Imprime" 
     SIZE 4 BY 1.25.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-carga.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-carga.bmp":U
     LABEL "Imprime" 
     SIZE 4 BY 1.25.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-etq-arquivo AT ROW 1.33 COL 54.72
     bt-diversas AT ROW 1.33 COL 59.57
     bt-imprime AT ROW 1.33 COL 66.57
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 8.21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-cadsi4
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsi4 ASSIGN
         HIDDEN             = YES
         TITLE              = "Manutená∆o Etiqueta Avulsa"
         HEIGHT             = 8.21
         WIDTH              = 90
         MAX-HEIGHT         = 20.71
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.71
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsi4 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-cadsi3.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsi4
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsi4)
THEN w-cadsi4:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsi4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsi4 w-cadsi4
ON END-ERROR OF w-cadsi4 /* Manutená∆o Etiqueta Avulsa */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
 RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsi4 w-cadsi4
ON WINDOW-CLOSE OF w-cadsi4 /* Manutená∆o Etiqueta Avulsa */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-diversas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-diversas w-cadsi4
ON CHOOSE OF bt-diversas IN FRAME f-cad /* Button 1 */
DO:
   RUN pi-etq-coletor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-etq-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-etq-arquivo w-cadsi4
ON CHOOSE OF bt-etq-arquivo IN FRAME f-cad /* Imprime */
DO:
  RUN esp-aux/imp-etq.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-cadsi4
ON CHOOSE OF bt-imprime IN FRAME f-cad /* Imprime */
DO:
  RUN pi-imprime IN h_v03im001.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-cadsi4
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-cadsi4
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-cancelar w-cadsi4
ON CHOOSE OF MENU-ITEM mi-cancelar /* Cancelar */
DO:
  RUN pi-cancelar IN h_p-cadsi2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-cadsi4
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-cadsi4
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-desfazer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-desfazer w-cadsi4
ON CHOOSE OF MENU-ITEM mi-desfazer /* Desfazer */
DO:
  RUN pi-desfazer IN h_p-cadsi2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-cadsi4
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-incluir w-cadsi4
ON CHOOSE OF MENU-ITEM mi-incluir /* Incluir */
DO:
  RUN pi-alterar IN h_p-cadsi2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-cadsi4
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-cadsi4
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-cadsi4
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-cadsi4
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-salvar w-cadsi4
ON CHOOSE OF MENU-ITEM mi-salvar /* Salvar */
DO:
  RUN pi-salvar IN h_p-cadsi2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-cadsi4
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-cadsi4
ON CHOOSE OF MENU-ITEM mi-ultimo /* Èltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-cadsi4
ON CHOOSE OF MENU-ITEM mi-va-para /* V† para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsi4 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-cadsi4  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-cadsi4.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-cadsi2 ).
       RUN set-position IN h_p-cadsi2 ( 1.33 , 27.29 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

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
             INPUT  'esvwr/v03cl005.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v03im001 ).
       RUN set-position IN h_v03im001 ( 2.75 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 6.25 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q03cl005.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = clzoom/z02cl005.w,
                     ProgVaPara = clgo/g03cl005.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q01im003 ).
       RUN set-position IN h_q01im003 ( 1.25 , 44.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */

       /* Links to SmartViewer h_v03im001. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsi2 , 'TableIO':U , h_v03im001 ).
       RUN add-link IN adm-broker-hdl ( h_q01im003 , 'Record':U , h_v03im001 ).

       /* Links to SmartQuery h_q01im003. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q01im003 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01im003 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01im003 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-etq-arquivo:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadsi2 ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-imprime:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v03im001 ,
             h_p-exihel , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-cadsi4  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-cadsi4  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsi4)
  THEN DELETE WIDGET w-cadsi4.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-cadsi4  _DEFAULT-ENABLE
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
  ENABLE rt-button bt-etq-arquivo bt-diversas bt-imprime 
      WITH FRAME f-cad IN WINDOW w-cadsi4.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-cadsi4.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-cadsi4 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-cadsi4 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-cadsi4 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

  run pi-before-initialize.

  {utp/ut9000.i "BCX0004WIN" "1.00.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etq-coletor w-cadsi4 
PROCEDURE pi-etq-coletor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR c-id          AS CHAR FORMAT "x(20)".
  DEF VAR c-arq-coletor AS CHAR FORMAT "x(30)".
  DEF VAR c-arq-saida   AS CHAR FORMAT "x(30)".
  DEF VAR c-arq-bat     AS CHAR.
  DEF VAR i-handle      AS INTEGER NO-UNDO.

  ASSIGN c-arq-coletor = SESSION:TEMP-DIRECTORY + 
                        "ETQ-" + c-seg-usuario + ".TXT"
        c-arq-saida = SESSION:TEMP-DIRECTORY + 
                        "ETQ-S-" + c-seg-usuario + ".TXT". /*'m:\ems204\coletor\teste\ETQ-' + c-seg-usuario + ".TXT".*/

  /* Baixa os dados do Coletor */
  IF SESSION:SET-WAIT-STATE("general":U) THEN.

  OS-DELETE SILENT VALUE(c-arq-coletor).
  IF SEARCH("C:\IMPROTEC\P220\P220.EXE") <> ? THEN DO.
    ASSIGN c-arq-bat = SESSION:TEMP-DIRECTORY + "p220.bat".
    OUTPUT STREAM s-bat TO VALUE(c-arq-bat).
       PUT STREAM s-bat
           "c:" SKIP
           "cd " SESSION:TEMP-DIRECTORY SKIP
           "C:\IMPROTEC\P220\P220.EXE R 1 1 "
           c-arq-coletor FORMAT "x(40)" SKIP.
    OUTPUT STREAM s-bat CLOSE.

    IF SEARCH(c-arq-bat) <> ? THEN DO.
       OS-COMMAND SILENT VALUE(c-arq-bat).
       PAUSE 5 NO-MESSAGE.
       REPEAT. 
          RUN FindWindowA (0, "IMODEM", OUTPUT i-handle).
          IF i-handle = 0 THEN LEAVE.
       END. 
       OS-DELETE SILENT VALUE(c-arq-bat). 
    END.
  END.
  IF SESSION:SET-WAIT-STATE("") THEN.

  IF SEARCH(c-arq-coletor) = ? THEN DO.
    MESSAGE "Arquivo n∆o foi Baixado do Coletor..." VIEW-AS ALERT-BOX.
    RETURN ERROR.
  END.

  OUTPUT STREAM s-coletor TO VALUE(c-arq-saida).
  INPUT STREAM e-coletor FROM VALUE(c-arq-coletor) NO-ECHO.
  REPEAT.                 
     SET STREAM e-coletor c-id.

     IF c-id = '' THEN NEXT.

     FIND bc-etiqueta WHERE
          bc-etiqueta.progressivo = c-id NO-LOCK NO-ERROR.
     IF NOT AVAIL bc-etiqueta THEN DO.
        PUT STREAM s-coletor
            c-id
            "N£mero da Etiqueta invalido"
            SKIP.
        NEXT.
     END.

     FOR EACH tt-erro:
         DELETE tt-erro.
     END.

     RUN bcp/bcx2000.p (INPUT ROWID(bc-etiqueta),
                        INPUT "IMA0001Q",
                        INPUT-OUTPUT TABLE tt-erro).

  END.
  OUTPUT STREAM s-coletor CLOSE.
  INPUT STREAM e-coletor CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-cadsi4  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-cadsi4, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadsi4 
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

