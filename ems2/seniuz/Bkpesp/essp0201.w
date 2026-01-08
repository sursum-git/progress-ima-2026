&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{include/i-prgvrs.i ESSP0201 2.04.00.000}


/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF BUFFER moeda FOR mgcad.moeda.
DEF BUFFER empresa FOR mgcad.empresa.

/* Local Variable Definitions ---                                       */
DEFINE VAR c-empresa LIKE empresa.razao-social.

DEF VAR l-todos-locais AS LOG.
DEF VAR l-normal       AS LOG.
DEF VAR l-amostra      AS LOG.
DEF VAR l-exporta      AS LOG.
DEF VAR l-exclusivo    AS LOG.
DEF VAR l-ind          AS LOG.
DEF VAR l-retalho      AS LOG.
DEF VAR l-bloqueada    AS LOG.
DEF VAR l-refat        AS LOG.
DEF VAR l-pilotagem    AS LOG.
DEF VAR l-outros       AS LOG.
DEF VAR c-local        AS CHAR.
DEF VAR c-tipo         AS CHAR FORMAT "x(30)".
DEF VAR i-lin          AS INT.
DEF VAR i-col          AS INT.
DEF VAR i-pag          AS INT.
DEF VAR c-rua-ini      AS CHAR NO-UNDO.
DEF VAR c-rua-fin      AS CHAR NO-UNDO.
DEF VAR c-doca-ini     AS CHAR NO-UNDO. 
DEF VAR c-doca-fin     AS CHAR NO-UNDO.
DEF VAR i-tipo-rel     AS INTEGER.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.
DEFINE VAR l-ok                AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-cadsim
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-imp-etq bt-imprime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tipo w-cadsim 
FUNCTION fn-tipo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
DEFINE VARIABLE h_q01es087 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01es087 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imp-etq AUTO-GO 
     IMAGE-UP FILE "image/im-f-dv.bmp":U
     LABEL "Btn 2" 
     SIZE 4 BY 1.25 TOOLTIP "Imprime uma Etiqueta".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21 TOOLTIP "Imprimir uma Faixa de Localizaá‰es / Etiquetas".

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-imp-etq AT ROW 1.29 COL 61.43 WIDGET-ID 4
     bt-imprime AT ROW 1.42 COL 56.86 WIDGET-ID 2
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.14 BY 11.75 WIDGET-ID 100.


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
         TITLE              = "Manutená∆o de Localizaá‰es"
         HEIGHT             = 11.75
         WIDTH              = 89.14
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
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Manutená∆o de Localizaá‰es */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Manutená∆o de Localizaá‰es */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imp-etq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imp-etq w-cadsim
ON CHOOSE OF bt-imp-etq IN FRAME f-cad /* Btn 2 */
DO:
   RUN pi-etiqueta IN h_v01es087. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-cadsim
ON CHOOSE OF bt-imprime IN FRAME f-cad
DO:
   ASSIGN l-todos-locais = YES
          l-normal       = NO
          l-amostra      = NO
          l-exporta      = NO
          l-exclusivo    = NO
          l-ind          = NO
          l-retalho      = NO
          l-bloqueada    = NO
          l-refat        = NO
          l-pilotagem    = NO
          l-outros       = NO
          l-ok           = NO.

   RUN esp/essp0201a.w (INPUT-OUTPUT l-todos-locais,   
                        INPUT-OUTPUT l-normal,   
                        INPUT-OUTPUT l-amostra,
                        INPUT-OUTPUT l-exporta,
                        INPUT-OUTPUT l-exclusivo,   
                        INPUT-OUTPUT l-ind,
                        INPUT-OUTPUT l-retalho,
                        INPUT-OUTPUT l-bloqueada,
                        INPUT-OUTPUT l-refat,
                        INPUT-OUTPUT l-pilotagem,
                        INPUT-OUTPUT l-outros,
                        INPUT-OUTPUT c-rua-ini,  
                        INPUT-OUTPUT c-rua-fin,  
                        INPUT-OUTPUT c-doca-ini, 
                        INPUT-OUTPUT c-doca-fin,
                        INPUT-OUTPUT i-tipo-rel,
                        INPUT-OUTPUT l-ok). 
   IF l-ok THEN DO.
      ASSIGN c-local = "".
      IF l-todos-locais = YES THEN
         ASSIGN c-local = "1,2,3,4,5,6,7,8,9,10,".
      ELSE DO:
        ASSIGN c-local = c-local + IF l-normal    = YES THEN "1," ELSE ",".
        ASSIGN c-local = c-local + IF l-amostra   = YES THEN "2," ELSE ",".
        ASSIGN c-local = c-local + IF l-exporta   = YES THEN "3," ELSE ",".
        ASSIGN c-local = c-local + IF l-exclusivo = YES THEN "4," ELSE ",".
        ASSIGN c-local = c-local + IF l-ind       = YES THEN "5," ELSE ",".
        ASSIGN c-local = c-local + IF l-retalho   = YES THEN "6," ELSE ",".
        ASSIGN c-local = c-local + IF l-bloqueada = YES THEN "7," ELSE ",".
        ASSIGN c-local = c-local + IF l-refat     = YES THEN "8," ELSE ",".
        ASSIGN c-local = c-local + IF l-pilotagem = YES THEN "9," ELSE ",".
        ASSIGN c-local = c-local + IF l-outros    = YES THEN "10," ELSE ",".
      END.
 
      IF i-tipo-rel = 1 THEN
         RUN pi-imprime.
      ELSE DO.
         FOR EACH ob-localiz  WHERE
                  LOOKUP(STRING(ob-localiz.tipo),c-local) > 0 AND 
                  ob-localiz.rua >= c-rua-ini AND         
                  ob-localiz.rua <= c-rua-fin AND         
                  ob-localiz.doca >= c-doca-ini AND       
                  ob-localiz.doca <= c-doca-fin NO-LOCK
             BREAK BY ob-localiz.rua
                   BY ob-localiz.doca.

             RUN pi-imp-etiqueta IN h_v01es087 (INPUT ob-localiz.rua,
                                                INPUT ob-localiz.doca).
         END.
      END.
   END.
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

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esvwr/v01es087.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01es087 ).
       RUN set-position IN h_v01es087 ( 2.79 , 1.86 ) NO-ERROR.
       /* Size in UIB:  ( 9.75 , 88.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q01es087.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = eszoom\z01es087.w,
                     ProgVaPara = esgo\g01es087.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q01es087 ).
       RUN set-position IN h_q01es087 ( 1.29 , 67.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 6.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'State':U , h_p-exihel ).

       /* Links to SmartViewer h_v01es087. */
       RUN add-link IN adm-broker-hdl ( h_p-cadsim , 'TableIO':U , h_v01es087 ).
       RUN add-link IN adm-broker-hdl ( h_q01es087 , 'Record':U , h_v01es087 ).
       RUN add-link IN adm-broker-hdl ( THIS-PROCEDURE , 'State':U , h_v01es087 ).

       /* Links to SmartQuery h_q01es087. */
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01es087 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01es087 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             bt-imp-etq:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-cadsim ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             h_p-cadsim , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v01es087 ,
             bt-imprime:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  ENABLE rt-button bt-imp-etq bt-imprime 
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

  {utp/ut9000.i "ESSP0201" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec w-cadsim 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  63
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  69
        "HORA: "                                  AT  97
        STRING(TIME,"hh:mm:ss")                   AT 103
        "PAG:"                                    AT 126
        i-pag FORMAT ">>>"                        AT 131
        SKIP(1).
    
    PUT "RELAT‡RIO DE LOCALIZAÄÂES" AT 58 SKIP(1). 


    PUT "RUA   DOCA  TIPO                 RUA   DOCA  TIPO                 RUA   DOCA  TIPO                 RUA   DOCA  TIPO" AT 1.
    PUT "----- ----- -----------------    ----- ----- -----------------    ----- ----- -----------------    ----- ----- -----------------" AT 1.

    ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime w-cadsim 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61.
          PUT CONTROL "~033E~033(s18H".   
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0183.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "IBM850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-lin       = 99
            i-col       =  1
            i-pag       =  1.

     FOR EACH ob-localiz  WHERE
              LOOKUP(STRING(ob-localiz.tipo),c-local) > 0 AND 
              ob-localiz.rua >= c-rua-ini AND         
              ob-localiz.rua <= c-rua-fin AND         
              ob-localiz.doca >= c-doca-ini AND       
              ob-localiz.doca <= c-doca-fin NO-LOCK
         BREAK BY ob-localiz.rua
               BY ob-localiz.doca.

         IF i-lin > 61 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.

         PUT ob-localiz.rua     FORMAT "x(5)"   AT i-col  
             ob-localiz.doca    FORMAT "x(5)"   AT i-col + 7
             fn-tipo()          FORMAT "X(20)"  AT i-col + 12.

         ASSIGN i-col = i-col + 33.

         IF i-col > 100 THEN DO.
            ASSIGN i-lin = i-lin + 1
                   i-col = 1.
            PUT SKIP.
         END.
         IF LAST-OF(ob-localiz.rua) THEN DO:
            IF i-col <> 1 THEN DO.
               PUT SKIP.
               ASSIGN i-lin = i-lin + 1.

            END.
            PUT "--------------------------------------------------------------------------------------------------------------------------------" AT 1.
            ASSIGN i-lin = i-lin + 1
                   i-col = 1.
         END.
     END.
     PUT SKIP(2).

     IF i-saida = 3 THEN DO.
        RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                              INPUT c-saida).
        DELETE PROCEDURE h-prog.
     END.
     OUTPUT CLOSE.
  END.
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tipo w-cadsim 
FUNCTION fn-tipo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  {esinc/i-dsrb.i ob-localiz.tipo ob-localiz.tipo c-tipo}.

  RETURN c-tipo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

