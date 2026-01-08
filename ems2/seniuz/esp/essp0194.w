&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-forma 
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

def var v-row-1 as rowid no-undo.
def var v-row-2 as rowid no-undo.
DEF VAR c-cod_mestre LIKE usuar_mestre.cod_usuario.
DEF VAR i-row        AS INT.

DEF TEMP-TABLE tt-work
    FIELD cod_usuario LIKE usuar_mestre.cod_usuario
    FIELD nom_usuario LIKE usuar_mestre.nom_usuario.

DEF TEMP-TABLE tt-hierarquia
    FIELD cod_mestre  LIKE hierarquia_usuar.cod_mestre
    FIELD cod_usuario LIKE hierarquia_usuar.cod_usuario
    FIELD nom_usuario LIKE hierarquia_usuar.nom_usuario.

DEF BUFFER b-usuar_mestre FOR usuar_mestre.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-forma
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-hierarquia

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-hierarquia tt-work

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-hierarquia                                 */
&Scoped-define FIELDS-IN-QUERY-br-hierarquia tt-hierarquia.cod_usuario tt-hierarquia.nom_usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-hierarquia   
&Scoped-define SELF-NAME br-hierarquia
&Scoped-define QUERY-STRING-br-hierarquia FOR EACH tt-hierarquia WHERE NO-LOCK                               BY tt-hierarquia.cod_mestre                               BY tt-hierarquia.cod_usuario
&Scoped-define OPEN-QUERY-br-hierarquia OPEN QUERY {&SELF-NAME} FOR EACH tt-hierarquia WHERE NO-LOCK                               BY tt-hierarquia.cod_mestre                               BY tt-hierarquia.cod_usuario.
&Scoped-define TABLES-IN-QUERY-br-hierarquia tt-hierarquia
&Scoped-define FIRST-TABLE-IN-QUERY-br-hierarquia tt-hierarquia


/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.cod_usuario tt-work.nom_usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define QUERY-STRING-br-work FOR EACH tt-work WHERE NO-LOCK                               BY tt-work.cod_usuario
&Scoped-define OPEN-QUERY-br-work OPEN QUERY {&SELF-NAME} FOR EACH tt-work WHERE NO-LOCK                               BY tt-work.cod_usuario.
&Scoped-define TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work


/* Definitions for FRAME f-cad                                          */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-button br-work br-hierarquia 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-forma AS WIDGET-HANDLE NO-UNDO.

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
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01un178 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v01un178 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "adeicon\next-au":U
     IMAGE-INSENSITIVE FILE "adeicon\next-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "adeicon\prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon\prev-ai":U
     LABEL "" 
     SIZE 7 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 12.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 88.57 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-hierarquia FOR 
      tt-hierarquia SCROLLING.

DEFINE QUERY br-work FOR 
      tt-work SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-hierarquia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-hierarquia w-forma _FREEFORM
  QUERY br-hierarquia NO-LOCK DISPLAY
      tt-hierarquia.cod_usuario  COLUMN-LABEL "Usuario" WIDTH 08
      tt-hierarquia.nom_usuario  COLUMN-LABEL "Nome"    WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 37 BY 12
         FONT 1
         TITLE "Usu rios do Grupo" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work w-forma _FREEFORM
  QUERY br-work NO-LOCK DISPLAY
      tt-work.cod_usuario  COLUMN-LABEL "Usuario" WIDTH 08
      tt-work.nom_usuario  COLUMN-LABEL "Nome"    WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 37 BY 12
         FONT 1
         TITLE "Usu rios Dispon¡veis" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     br-work AT ROW 5.38 COL 3.72
     br-hierarquia AT ROW 5.38 COL 50.14
     bt-add AT ROW 8.75 COL 41.72
     bt-del AT ROW 10.38 COL 41.72
     RECT-1 AT ROW 5 COL 1.43
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-forma
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-forma ASSIGN
         HIDDEN             = YES
         TITLE              = "Manuten‡Æo Hierarquia Usuario"
         HEIGHT             = 16.88
         WIDTH              = 90
         MAX-HEIGHT         = 20.67
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.67
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-forma 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-consim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-forma
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-work rt-button f-cad */
/* BROWSE-TAB br-hierarquia br-work f-cad */
/* SETTINGS FOR BUTTON bt-add IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-del IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-forma)
THEN w-forma:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-hierarquia
/* Query rebuild information for BROWSE br-hierarquia
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-hierarquia WHERE NO-LOCK
                              BY tt-hierarquia.cod_mestre
                              BY tt-hierarquia.cod_usuario.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-hierarquia */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-work WHERE NO-LOCK
                              BY tt-work.cod_usuario.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-work */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-forma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-forma w-forma
ON END-ERROR OF w-forma /* Manuten‡Æo Hierarquia Usuario */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-forma w-forma
ON WINDOW-CLOSE OF w-forma /* Manuten‡Æo Hierarquia Usuario */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-work
&Scoped-define SELF-NAME br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work w-forma
ON VALUE-CHANGED OF br-work IN FRAME f-cad /* Usu rios Dispon¡veis */
DO:
   {&OPEN-QUERY-br-hierarquia}
   APPLY 'value-changed' TO br-hierarquia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add w-forma
ON CHOOSE OF bt-add IN FRAME f-cad
DO:
   DO i-row = 1 TO br-work:NUM-SELECTED-ROWS.
      IF br-work:FETCH-SELECTED-ROW(i-row) THEN DO.

         FIND tt-hierarquia WHERE
              tt-hierarquia.cod_mestre = c-cod_mestre AND 
              tt-hierarquia.cod_usuario = tt-work.cod_usuario NO-ERROR.
         IF NOT AVAIL tt-hierarquia THEN DO.
            CREATE tt-hierarquia.
            ASSIGN tt-hierarquia.cod_mestre  = c-cod_mestre
                   tt-hierarquia.cod_usuario = tt-work.cod_usuario
                   tt-hierarquia.nom_usuario = tt-work.nom_usuario.
            CREATE hierarquia.
            ASSIGN hierarquia.cod_mestre  = c-cod_mestre
                   hierarquia.cod_usuario = tt-work.cod_usuario
                   hierarquia.nom_usuario = tt-work.nom_usuario.

            DELETE tt-work.
         END.
      END.
   END.

   {&OPEN-QUERY-br-work}
   {&OPEN-QUERY-br-hierarquia}

   ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   IF NUM-RESULTS("br-work") = 0 THEN
      ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   IF NUM-RESULTS("br-hierarquia") = 0 THEN
      ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del w-forma
ON CHOOSE OF bt-del IN FRAME f-cad
DO:
   DO i-row = 1 TO br-hierarquia:NUM-SELECTED-ROWS.
      IF br-hierarquia:FETCH-SELECTED-ROW(i-row) THEN DO.

         FIND tt-work WHERE
              tt-work.cod_usuario = tt-hierarquia.cod_usuario NO-ERROR.
         IF NOT AVAIL tt-work THEN DO.
            CREATE tt-work.
            ASSIGN tt-work.cod_usuario = tt-hierarquia.cod_usuario
                   tt-work.nom_usuario = tt-hierarquia.nom_usuario.
            FIND hierarquia WHERE
                 hierarquia.cod_mestre  = c-cod_mestre AND
                 hierarquia.cod_usuario = tt-hierarquia.cod_usuario SHARE-LOCK NO-ERROR.
            IF AVAIL hierarquia THEN
               DELETE hierarquia.
            DELETE tt-hierarquia.
         END.
      END.
   END.

   {&OPEN-QUERY-br-work}
   {&OPEN-QUERY-br-hierarquia}

   ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   IF NUM-RESULTS("br-work") = 0 THEN
      ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   IF NUM-RESULTS("br-hierarquia") = 0 THEN
      ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-forma
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-forma
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-forma
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-forma
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-forma
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-forma
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-forma
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-forma
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-forma
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-forma
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-forma
ON CHOOSE OF MENU-ITEM mi-ultimo /* éltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-forma
ON CHOOSE OF MENU-ITEM mi-va-para /* V  para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-hierarquia
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-forma 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

ON 'F5':U OF br-work DO:
    MESSAGE "Rodou"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   {&OPEN-QUERY-br-work}
   APPLY 'value-changed' TO br-work.
END.



STATUS INPUT OFF. /* Desliga Mensagem no Rodap‚ da Tela */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-forma  _ADM-CREATE-OBJECTS
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
             INPUT  'esvwr/v01un178.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v01un178 ).
       RUN set-position IN h_v01un178 ( 3.00 , 1.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.75 , 88.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'esqry/q01un178.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q01un178 ).
       RUN set-position IN h_q01un178 ( 1.42 , 52.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.00 , 7.00 ) */

       /* Links to SmartViewer h_v01un178. */
       RUN add-link IN adm-broker-hdl ( h_q01un178 , 'Record':U , h_v01un178 ).

       /* Links to SmartQuery h_q01un178. */
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01un178 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navega ,
             br-work:HANDLE IN FRAME f-cad , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             h_p-navega , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v01un178 ,
             h_p-exihel , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-forma  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-forma  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-forma)
  THEN DELETE WIDGET w-forma.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-forma  _DEFAULT-ENABLE
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
  ENABLE RECT-1 rt-button br-work br-hierarquia 
      WITH FRAME f-cad IN WINDOW w-forma.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-forma.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-forma 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse w-forma 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-cod_usuario LIKE usuar_mestre.cod_usuario.

  ASSIGN c-cod_mestre = p-cod_usuario.

  br-hierarquia:TITLE IN FRAME {&FRAME-NAME} = "Usu rios do Grupo " + c-cod_mestre.

  FOR EACH tt-work.
      DELETE tt-work.
  END.

  FOR EACH tt-hierarquia.
      DELETE tt-hierarquia.
  END.

  FOR EACH b-usuar_mestre WHERE
           b-usuar_mestre.cod_usuario <> c-cod_mestre NO-LOCK.

      FIND hierarquia_usuar WHERE
           hierarquia_usuar.cod_mestre = c-cod_mestre AND
           hierarquia_usuar.cod_usuario = b-usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
      IF AVAIL hierarquia_usuar THEN NEXT.

      FIND tt-work WHERE
           tt-work.cod_usuario = b-usuar_mestre.cod_usuario NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-work THEN DO:
         CREATE tt-work.
         ASSIGN tt-work.cod_usuario = b-usuar_mestre.cod_usuario
                tt-work.nom_usuario = b-usuar_mestre.nom_usuario.
      END.
  END.

  FOR EACH hierarquia_usuar WHERE
           hierarquia_usuar.cod_mestre = c-cod_mestre NO-LOCK.

      FIND tt-hierarquia WHERE
           tt-hierarquia.cod_mestre  = hierarquia.cod_mestre  AND
           tt-hierarquia.cod_usuario = hierarquia.cod_usuario NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-hierarquia THEN DO:
         CREATE tt-hierarquia.
         ASSIGN tt-hierarquia.cod_mestre  = c-cod_mestre
                tt-hierarquia.cod_usuario = hierarquia.cod_usuario
                tt-hierarquia.nom_usuario = hierarquia.nom_usuario.
      END.
  END.

  {&OPEN-QUERY-br-work}
  {&OPEN-QUERY-br-hierarquia}

  IF NUM-RESULTS("br-work") > 0 THEN
     ASSIGN bt-add:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  IF NUM-RESULTS("br-hierarquia") > 0 THEN
     ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  APPLY 'value-changed' TO br-work IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-forma  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-work"}
  {src/adm/template/snd-list.i "tt-hierarquia"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-forma 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  if p-state = "incluir-browse" then do:
  /*
    run pi-posicao-query  in h_q-<query>(output v-row-1).
    run pi-posicao-browse in h_b-<browse>(output v-row-2).
    run pi-add-browse     in h_b-<browse-formacao>(input v-row-1, input v-row-2).
    RUN dispatch IN h_b-<browse-formacao> ('open-query':U).
  */
  end.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

