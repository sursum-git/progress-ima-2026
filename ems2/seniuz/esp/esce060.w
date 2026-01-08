&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESCE060 2.04.00.001}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE l-elimina AS LOGICAL  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-confirma i-nr-ord-produ-ini ~
i-nr-ord-produ-fim i-tipo-ordem-ini i-tipo-ordem-fim dt-trans-ini ~
dt-trans-fim c-esp-docto-ini c-esp-docto-fim c-serie-docto-ini ~
c-serie-docto-fim c-observacao IMAGE-1 IMAGE-2 IMAGE-39 IMAGE-40 IMAGE-41 ~
IMAGE-42 IMAGE-43 IMAGE-44 IMAGE-45 IMAGE-46 RECT-14 rt-button 
&Scoped-Define DISPLAYED-OBJECTS i-nr-ord-produ-ini i-nr-ord-produ-fim ~
i-tipo-ordem-ini i-tipo-ordem-fim dt-trans-ini dt-trans-fim c-esp-docto-ini ~
c-esp-docto-fim c-serie-docto-ini c-serie-docto-fim c-observacao 

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
       SUB-MENU  mi-programa    LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "" 
     SIZE 5 BY 1.13.

DEFINE VARIABLE c-esp-docto-fim AS CHARACTER FORMAT "X(03)":U INITIAL "37" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "ACA","1",
                     "ACT","2",
                     "NU1","3",
                     "DD","4",
                     "DEV","5",
                     "DIV","6",
                     "DRM","7",
                     "EAC","8",
                     "EGF","9",
                     "BEM","10",
                     "NU2","11",
                     "NU3","12",
                     "NU4","13",
                     "ICM","14",
                     "INV","15",
                     "IPL","16",
                     "MOB","17",
                     "NC","18",
                     "NF","19",
                     "NFD","20",
                     "NFE","21",
                     "NFS","22",
                     "NFT","23",
                     "NU5","24",
                     "REF","25",
                     "RCS","26",
                     "RDD","27",
                     "REQ","28",
                     "RFS","29",
                     "RM","30",
                     "RRQ","31",
                     "STR","32",
                     "TRA","33",
                     "ZZZ","34",
                     "SOB","35",
                     "EDD","36",
                     "VAR","37"
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE c-esp-docto-ini AS CHARACTER FORMAT "X(03)":U INITIAL "1" 
     LABEL "Esp‚cie" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "ACA","1",
                     "ACT","2",
                     "NU1","3",
                     "DD","4",
                     "DEV","5",
                     "DIV","6",
                     "DRM","7",
                     "EAC","8",
                     "EGF","9",
                     "BEM","10",
                     "NU2","11",
                     "NU3","12",
                     "NU4","13",
                     "ICM","14",
                     "INV","15",
                     "IPL","16",
                     "MOB","17",
                     "NC","18",
                     "NF","19",
                     "NFD","20",
                     "NFE","21",
                     "NFS","22",
                     "NFT","23",
                     "NU5","24",
                     "REF","25",
                     "RCS","26",
                     "RDD","27",
                     "REQ","28",
                     "RFS","29",
                     "RM","30",
                     "RRQ","31",
                     "STR","32",
                     "TRA","33",
                     "ZZZ","34",
                     "SOB","35",
                     "EDD","36",
                     "VAR","37"
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE i-tipo-ordem-fim AS CHARACTER FORMAT "X(20)":U INITIAL "9" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1 - Interna","1",
                     "2 - Externa","2",
                     "3 - Interna/Externa","3",
                     "4 - Retrabalho","4",
                     "5 - Conserto","5",
                     "6 - Manuten‡Æo","6",
                     "7 - Ativo Fixo","7",
                     "8 - Ferramentaria","8",
                     "9 - Reaproveitamento","9"
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE i-tipo-ordem-ini AS CHARACTER FORMAT "X(20)":U INITIAL "1" 
     LABEL "Tipo Ordem" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "1 - Interna","1",
                     "2 - Externa","2",
                     "3 - Interna/Externa","3",
                     "4 - Retrabalho","4",
                     "5 - Conserto","5",
                     "6 - Manuten‡Æo","6",
                     "7 - Ativo Fixo","7",
                     "8 - Ferramentaria","8",
                     "9 - Reaproveitamento","9"
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE c-observacao AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 86.57 BY 5
     FONT 0 NO-UNDO.

DEFINE VARIABLE c-serie-docto-fim AS CHARACTER FORMAT "x(5)" INITIAL "ZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE c-serie-docto-ini AS CHARACTER FORMAT "x(5)" 
     LABEL "S‚rie Documento":R18 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88.

DEFINE VARIABLE dt-trans-fim AS DATE FORMAT "99/99/9999" INITIAL 02/10/05 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE dt-trans-ini AS DATE FORMAT "99/99/9999" INITIAL 02/10/05 
     LABEL "Data In¡cio":R17 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88.

DEFINE VARIABLE i-nr-ord-produ-fim AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 999999999 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88.

DEFINE VARIABLE i-nr-ord-produ-ini AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     LABEL "Ordem Produ‡Æo":R17 
     VIEW-AS FILL-IN 
     SIZE 13.72 BY .88.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-41
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-42
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-43
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-44
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-45
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-46
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 11.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-confirma AT ROW 1.25 COL 65.29
     i-nr-ord-produ-ini AT ROW 3.25 COL 20 COLON-ALIGNED HELP
          "N£mero da Ordem de Produ‡Æo"
     i-nr-ord-produ-fim AT ROW 3.25 COL 55 COLON-ALIGNED HELP
          "N£mero da Ordem de Produ‡Æo" NO-LABEL
     i-tipo-ordem-ini AT ROW 4.25 COL 20 COLON-ALIGNED
     i-tipo-ordem-fim AT ROW 4.25 COL 55 COLON-ALIGNED NO-LABEL
     dt-trans-ini AT ROW 5.25 COL 20 COLON-ALIGNED
     dt-trans-fim AT ROW 5.25 COL 55 COLON-ALIGNED NO-LABEL
     c-esp-docto-ini AT ROW 6.25 COL 20 COLON-ALIGNED
     c-esp-docto-fim AT ROW 6.25 COL 55 COLON-ALIGNED NO-LABEL
     c-serie-docto-ini AT ROW 7.25 COL 20 COLON-ALIGNED
     c-serie-docto-fim AT ROW 7.25 COL 55 COLON-ALIGNED NO-LABEL
     c-observacao AT ROW 9.25 COL 2.43 NO-LABEL
     IMAGE-1 AT ROW 3.25 COL 43
     IMAGE-2 AT ROW 3.25 COL 52.43
     IMAGE-39 AT ROW 4.25 COL 43
     IMAGE-40 AT ROW 4.25 COL 52.43
     IMAGE-41 AT ROW 5.25 COL 43
     IMAGE-42 AT ROW 5.25 COL 52.43
     IMAGE-43 AT ROW 6.25 COL 43
     IMAGE-44 AT ROW 6.25 COL 52.43
     IMAGE-45 AT ROW 7.25 COL 43
     IMAGE-46 AT ROW 7.25 COL 52.43
     RECT-14 AT ROW 2.75 COL 1
     rt-button AT ROW 1 COL 1
     "ATEN€ÇO!!!" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 8.5 COL 3
          FGCOLOR 12 FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
         FONT 1.


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
         TITLE              = "Elimina Ordens de Produ‡Æo"
         HEIGHT             = 13.75
         WIDTH              = 90
         MAX-HEIGHT         = 21.75
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 21.75
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
   L-To-R                                                               */
ASSIGN 
       c-observacao:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Elimina Ordens de Produ‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Elimina Ordens de Produ‡Æo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-livre
ON CHOOSE OF bt-confirma IN FRAME f-cad
DO:
  
    IF INPUT i-nr-ord-produ-ini = 0 OR
       INPUT i-nr-ord-produ-fim > 999999 THEN DO:
        MESSAGE "Informe intervalo correto de n£meros da ordem de produ‡Æo!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN 'ADM-ERROR':U.
    END.

    ASSIGN INPUT i-nr-ord-produ-ini
                 i-nr-ord-produ-fim
                 i-tipo-ordem-ini
                 i-tipo-ordem-fim
                 dt-trans-ini
                 dt-trans-fim
                 c-esp-docto-ini
                 c-esp-docto-fim
                 c-serie-docto-ini
                 c-serie-docto-fim.

    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST param-estoq NO-LOCK NO-ERROR.

    /*
    IF dt-trans-ini <= param-estoq.mensal-ate THEN DO:
        RUN utp/ut-msg-liasa.p (INPUT 1,
                                INPUT "Per¡odo Encerrado.!",
                                INPUT "Eliminacao inv lida. Per¡odo encerrado ou pre‡o m‚dio j  calculado.").
        RETURN 'ADM-ERROR':U.
    END.

    RUN utp/ut-msg-liasa.p (INPUT 3,
                            INPUT "Confirma Elimina‡Æo?",
                            INPUT "Ao confirmar a elimina‡Æo os dados nÆo poderÆo ser recuperados.").


    IF RETURN-VALUE = "no" THEN
        RETURN 'ADM-ERROR':U.
    */

    SESSION:SET-WAIT-STATE("general":U).
    RUN pi-elimina.
    SESSION:SET-WAIT-STATE("":U).
    
    IF NOT l-elimina THEN
        MESSAGE "Nenhuma Ordem Foi Eliminada..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE MESSAGE "Processamento Finalizado..."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.

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
ON MENU-DROP OF MENU mi-programa /* Arquivo */
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
             bt-confirma:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY i-nr-ord-produ-ini i-nr-ord-produ-fim i-tipo-ordem-ini 
          i-tipo-ordem-fim dt-trans-ini dt-trans-fim c-esp-docto-ini 
          c-esp-docto-fim c-serie-docto-ini c-serie-docto-fim c-observacao 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-confirma i-nr-ord-produ-ini i-nr-ord-produ-fim i-tipo-ordem-ini 
         i-tipo-ordem-fim dt-trans-ini dt-trans-fim c-esp-docto-ini 
         c-esp-docto-fim c-serie-docto-ini c-serie-docto-fim c-observacao 
         IMAGE-1 IMAGE-2 IMAGE-39 IMAGE-40 IMAGE-41 IMAGE-42 IMAGE-43 IMAGE-44 
         IMAGE-45 IMAGE-46 RECT-14 rt-button 
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

  {utp/ut9000.i "ESCE060" "2.04.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN c-observacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
            "Este programa tem por objetivo estornar e eliminar os movimentos de " +
            "estoque e os movimentos de materiais de uma ordem de produ‡Æo. " + CHR(10) +
            "Dever  ser tomado o m ximo de cuidado no momento da sele‡Æo, pois este programa " +
            "ir  eliminar os movimentos fisicamente da base de dados." + CHR(10) + CHR(10) +
            "    SEMPRE FACA BACKUP ANTES DE QUALQUER ACERTO EM SUA BASE DE DADOS  ".

  APPLY "ENTRY" TO i-nr-ord-produ-ini IN FRAME {&FRAME-NAME}.

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-elimina w-livre 
PROCEDURE pi-elimina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN l-elimina = NO.

    FOR EACH ord-prod WHERE 
             ord-prod.nr-ord-produ  >= i-nr-ord-produ-ini AND 
             ord-prod.nr-ord-produ  <= i-nr-ord-produ-fim AND 
             ord-prod.tipo          >= INTEGER(i-tipo-ordem-ini) AND 
             ord-prod.tipo          <= INTEGER(i-tipo-ordem-fim) EXCLUSIVE-LOCK:

        IF ord-prod.valorizada = YES THEN 
            NEXT.

        FIND FIRST ord-manut WHERE
                   ord-manut.nr-ord-prod = ord-prod.nr-ord-prod
                   NO-LOCK NO-ERROR.
        IF AVAILABLE ord-manut THEN
            NEXT.

        FOR EACH movto-estoq WHERE 
                 movto-estoq.nr-ord-produ = ord-prod.nr-ord-produ AND
                 movto-estoq.num-sequen   > 0 AND 
                 movto-estoq.dt-trans     >= dt-trans-ini AND 
                 movto-estoq.dt-trans     <= dt-trans-fim AND
                 movto-estoq.esp-docto    >= INTEGER(c-esp-docto-ini) AND 
                 movto-estoq.esp-docto    <= INTEGER(c-esp-docto-fim) AND 
                 movto-estoq.serie        >= c-serie-docto-ini AND
                 movto-estoq.serie        <= c-serie-docto-fim EXCLUSIVE-LOCK:

            DO:
                FIND item WHERE
                     item.it-codigo = movto-estoq.it-codigo NO-ERROR.

                FIND item-estab WHERE
                     item-estab.it-codigo   = movto-estoq.it-codigo AND
                     item-estab.cod-estabel = movto-estoq.cod-estabel
                     NO-ERROR.
    
                IF NOT CAN-DO("4,1",STRING(item.tipo-contr)) THEN DO:
                    IF movto-estoq.esp-docto <> 1 AND
                       movto-estoq.esp-docto <> 8 AND
                       movto-estoq.esp-docto <> 27 THEN DO:

                        FIND conta-contab WHERE
                             conta-contab.ep-codigo = param-global.empresa-prin AND 
                             conta-contab.ct-codigo = movto-estoq.ct-codigo AND 
                             conta-contab.sc-codigo = movto-estoq.sc-codigo
                             NO-LOCK NO-ERROR.

                        IF AVAILABLE conta-contab AND
                           conta-contab.estoque = 1 THEN
                            IF movto-estoq.tipo-trans = 1 THEN
                                ASSIGN item.consumo-aad       = item.consumo-aad + movto-estoq.quantidade
                                       item-estab.consumo-aad = item-estab.consumo-aad + movto-estoq.quantidade.
                            ELSE 
                                ASSIGN item.consumo-aad       = item.consumo-aad - movto-estoq.quantidade
                                       item-estab.consumo-aad = item-estab.consumo-aad - movto-estoq.quantidade.
                    END.
                END.

                IF item.tipo-contr        <> 4 AND 
                   movto-estoq.quantidade <> 0 THEN DO:

                    FIND saldo-estoq WHERE
                         saldo-estoq.it-codigo = movto-estoq.it-codigo AND
                         saldo-estoq.cod-estabel = movto-estoq.cod-estabel AND
                         saldo-estoq.cod-depos = movto-estoq.cod-depos AND
                         saldo-estoq.cod-localiz = movto-estoq.cod-localiz AND
                         saldo-estoq.lote = movto-estoq.lote AND
                         saldo-estoq.cod-refer = movto-estoq.cod-refer 
                         NO-ERROR.
                    IF AVAILABLE saldo-estoq THEN DO:
                        IF movto-estoq.tipo-trans = 1 THEN 
                            ASSIGN saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu - movto-estoq.quantidade.
                        ELSE ASSIGN saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu + movto-estoq.quantidade.
                    END.
                END.
            END.

            FIND FIRST movto-mat USE-INDEX num-seq WHERE
                       movto-mat.nr-ord-prod = movto-estoq.nr-ord-produ AND
                       movto-mat.num-sequen = movto-estoq.num-sequen NO-ERROR.
            IF AVAILABLE movto-mat THEN DO:
                DELETE movto-mat VALIDATE(TRUE,"").
            END.
            DELETE movto-estoq VALIDATE(TRUE,"").
        END. /*for each movto-estoq*/


        FOR EACH movto-ggf WHERE 
                 movto-ggf.nr-ord-prod  = ord-prod.nr-ord-prod AND
                 movto-ggf.dt-trans    >= dt-trans-ini AND
                 movto-ggf.dt-trans    <= dt-trans-fim EXCLUSIVE-LOCK:

            DELETE movto-ggf VALIDATE(TRUE,"").

        END.

        FIND FIRST movto-estoq WHERE
                   movto-estoq.nr-ord-produ = ord-prod.nr-ord-produ AND
                   movto-estoq.num-sequen   > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE movto-estoq THEN DO:
            FIND FIRST movto-ggf WHERE
                       movto-ggf.nr-ord-prod = ord-prod.nr-ord-prod
                       NO-LOCK NO-ERROR.
            IF NOT AVAIL movto-ggf THEN DO:

                FOR EACH ext-ord WHERE 
                         ext-ord.nr-ord-prod = ord-prod.nr-ord-prod EXCLUSIVE-LOCK:
                    DELETE ext-ord VALIDATE(TRUE,"").
                END. 

                FIND ord-manut WHERE 
                     ord-manut.nr-ord-prod = ord-prod.nr-ord-prod NO-ERROR.
                IF AVAILABLE ord-manut THEN  
                    DELETE ord-manut VALIDATE(TRUE,"").
    
                IF ord-prod.nr-req-sum <> 0 THEN
                    FOR EACH req-sum WHERE 
                             req-sum.nr-req-sum = ord-prod.nr-req-sum:
                        DELETE req-sum validate(true,"").
                    END.
    
                FOR each reservas WHERE 
                         reservas.nr-ord-prod = ord-prod.nr-ord-prod:
                    DELETE reservas validate(true,"").
                END.
    
                FOR EACH oper-ord WHERE
                         oper-ord.nr-ord-prod = ord-prod.nr-ord-prod:
                    DELETE oper-ord validate(true,"").
                END.
    
                FOR EACH pert-ordem WHERE
                         pert-ordem.nr-ord-prod = ord-prod.nr-ord-prod:
                    DELETE pert-ordem VALIDATE(TRUE,"").
                END.
    
                FOR EACH ord-rep WHERE 
                         ord-rep.nr-ord-prod = ord-prod.nr-ord-prod:
                    FOR EACH rep-prod WHERE 
                             rep-prod.nr-reporte = ord-rep.nr-reporte:
                        DELETE rep-prod validate(true,"").
                    END.
                    DELETE ord-rep validate(true,"").
                END.

                FOR EACH ref-ordem WHERE 
                         ref-ordem.nr-ord-produ = ord-prod.nr-ord-produ EXCLUSIVE-LOCK:
                    DELETE ref-ordem.
                END.
    
                FOR EACH aloca-reserva WHERE 
                         aloca-reserva.nr-ord-produ = ord-prod.nr-ord-produ:
                    DELETE aloca-reserva.
                END.
                DELETE ord-prod VALIDATE(TRUE,"").

                ASSIGN l-elimina = YES.

            END.
        END.
    END.

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

