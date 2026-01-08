&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESFIN010 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i <programa> <m¢dulo>}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttTitulos
    FIELD rRowid        AS ROWID
    FIELD codEstab      AS CHAR
    FIELD codSerie      AS CHAR
    FIELD codEspec      AS CHAR
    FIELD codTitAp      AS CHAR
    FIELD codFornec     AS INT
    FIELD codParcela    AS CHAR
    FIELD valor         AS DECIMAL
    FIELD acao          AS CHAR
    FIELD origem        AS CHAR
    FIELD vlAjuste      AS DECIMAL
    FIELD rRowidAjuste  AS ROWID
    FIELD rRowidExcluir AS ROWID .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brTits

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTitulos

/* Definitions for BROWSE brTits                                        */
&Scoped-define FIELDS-IN-QUERY-brTits codEstab codSerie codEspec codTitAp codFornec codParcela valor acao origem vlAjuste   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTits   
&Scoped-define SELF-NAME brTits
&Scoped-define QUERY-STRING-brTits FOR EACH ttTitulos
&Scoped-define OPEN-QUERY-brTits OPEN QUERY {&SELF-NAME} FOR EACH ttTitulos .
&Scoped-define TABLES-IN-QUERY-brTits ttTitulos
&Scoped-define FIRST-TABLE-IN-QUERY-brTits ttTitulos


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brTits}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiCodEstabIni fiCodEstabFim fiDtEmisIni ~
fiDtEmisFim fiCodEmitIni fiCodEmitFim fiCodTitApIni fiCodTitApFim ~
fiCodEspIni fiCodEspFim fiTipoFluxoOrig fiTipoFluxoNovo btExec rt-button ~
RECT-1 brTits RECT-2 
&Scoped-Define DISPLAYED-OBJECTS fiCodEstabIni fiCodEstabFim fiDtEmisIni ~
fiDtEmisFim fiCodEmitIni fiCodEmitFim fiCodTitApIni fiCodTitApFim ~
fiCodEspIni fiCodEspFim fiTipoFluxoOrig fiTipoFluxoNovo 

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
     LABEL "Substituir" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiCodEmitFim AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 999999 
     LABEL "Emitente Fim" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEmitIni AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Emite Ini" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEspFim AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" 
     LABEL "Espec. Fim" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEspIni AS CHARACTER FORMAT "X(5)":U 
     LABEL "Espec. Ini" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEstabFim AS CHARACTER FORMAT "X(3)":U INITIAL "501" 
     LABEL "Estab.Fim" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodEstabIni AS CHARACTER FORMAT "X(3)":U INITIAL "501" 
     LABEL "Estab.Ini" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodTitApFim AS CHARACTER FORMAT "X(20)":U INITIAL "zzzzzzzzzzz" 
     LABEL "T¡tulo Fim" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiCodTitApIni AS CHARACTER FORMAT "X(20)":U 
     LABEL "Titulo Ini" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtEmisFim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/22 
     LABEL "Dt.Emis.Ini" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiDtEmisIni AS DATE FORMAT "99/99/9999":U INITIAL 01/01/22 
     LABEL "Dt.Emis.Ini" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .79 NO-UNDO.

DEFINE VARIABLE fiTipoFluxoNovo AS CHARACTER FORMAT "X(10)":U 
     LABEL "TP. Fluxo Novo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE VARIABLE fiTipoFluxoOrig AS CHARACTER FORMAT "X(10)":U 
     LABEL "TP.Fluxo Original" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 5.04.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 101 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTits FOR 
      ttTitulos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTits
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTits w-livre _FREEFORM
  QUERY brTits DISPLAY
      codEstab   COLUMN-LABEL "Estab."      
 codSerie   COLUMN-LABEL "Serie"   
 codEspec   COLUMN-LABEL "Espec."   
 codTitAp   COLUMN-LABEL "T¡tulo" FORMAT 'x(12)'   
 codFornec  COLUMN-LABEL "Fornec."  
 codParcela COLUMN-LABEL "Parc." 
 valor      COLUMN-LABEL "Valor" 
 acao       COLUMN-LABEL "A‡Æo"
 origem     COLUMN-LABEL "Origem"
 vlAjuste   COLUMN-LABEL "Vl.Ajuste"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94.29 BY 9.25
         FONT 1
         TITLE "T¡tulos Substituidos" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiCodEstabIni AT ROW 2.96 COL 18.57 COLON-ALIGNED WIDGET-ID 8
     fiCodEstabFim AT ROW 2.96 COL 47.72 COLON-ALIGNED WIDGET-ID 10
     fiDtEmisIni AT ROW 4 COL 18.57 COLON-ALIGNED WIDGET-ID 14
     fiDtEmisFim AT ROW 4 COL 47.86 COLON-ALIGNED WIDGET-ID 16
     fiCodEmitIni AT ROW 4.92 COL 18.57 COLON-ALIGNED WIDGET-ID 32
     fiCodEmitFim AT ROW 4.92 COL 48 COLON-ALIGNED WIDGET-ID 30
     fiCodTitApIni AT ROW 5.79 COL 18.57 COLON-ALIGNED WIDGET-ID 36
     fiCodTitApFim AT ROW 5.79 COL 48 COLON-ALIGNED WIDGET-ID 34
     fiCodEspIni AT ROW 6.63 COL 18.57 COLON-ALIGNED WIDGET-ID 26
     fiCodEspFim AT ROW 6.63 COL 48 COLON-ALIGNED WIDGET-ID 28
     fiTipoFluxoOrig AT ROW 4.08 COL 82 COLON-ALIGNED WIDGET-ID 2
     fiTipoFluxoNovo AT ROW 5.08 COL 82 COLON-ALIGNED WIDGET-ID 4
     btExec AT ROW 7.75 COL 6 WIDGET-ID 6
     brTits AT ROW 9 COL 6.29 WIDGET-ID 200
     "DE/PARA" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 2.58 COL 70.29 WIDGET-ID 24
     "FILTRO" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 2.5 COL 9 WIDGET-ID 20
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.71 COL 6 WIDGET-ID 18
     RECT-2 AT ROW 2.75 COL 69.14 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.08
         SIZE 112 BY 21.29
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
         TITLE              = "Altera‡Æo de Tipo de Fluxo"
         HEIGHT             = 19.04
         WIDTH              = 101.57
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brTits RECT-1 f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTits
/* Query rebuild information for BROWSE brTits
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTitulos .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brTits */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Altera‡Æo de Tipo de Fluxo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Altera‡Æo de Tipo de Fluxo */
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
ON CHOOSE OF btExec IN FRAME f-cad /* Substituir */
DO:
  EMPTY TEMP-TABLE ttTitulos.
  RUN getTitulosTpFluxo.
  /*MESSAGE 'antes setAlteracao'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
  RUN setAlteracoes.
  {&OPEN-QUERY-brTits}
  MESSAGE 'Processado'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCodEmitIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCodEmitIni w-livre
ON LEAVE OF fiCodEmitIni IN FRAME f-cad /* Emite Ini */
DO:
  IF SELF:SCREEN-VALUE <> '0'   THEN
     ASSIGN fiCodEmitFim:SCREEN-VALUE = SELF:SCREEN-VALUE . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCodEspIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCodEspIni w-livre
ON LEAVE OF fiCodEspIni IN FRAME f-cad /* Espec. Ini */
DO:
  IF SELF:SCREEN-VALUE <> ''   THEN
     ASSIGN fiCodEspFim:SCREEN-VALUE = SELF:SCREEN-VALUE . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCodTitApIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCodTitApIni w-livre
ON LEAVE OF fiCodTitApIni IN FRAME f-cad /* Titulo Ini */
DO:
  IF SELF:SCREEN-VALUE <> ''   THEN
     ASSIGN fiCodTitApFim:SCREEN-VALUE = SELF:SCREEN-VALUE . 
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


&Scoped-define BROWSE-NAME brTits
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
       RUN set-position IN h_p-exihel ( 1.13 , 85.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiCodEstabIni:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fiCodEstabIni fiCodEstabFim fiDtEmisIni fiDtEmisFim fiCodEmitIni 
          fiCodEmitFim fiCodTitApIni fiCodTitApFim fiCodEspIni fiCodEspFim 
          fiTipoFluxoOrig fiTipoFluxoNovo 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE fiCodEstabIni fiCodEstabFim fiDtEmisIni fiDtEmisFim fiCodEmitIni 
         fiCodEmitFim fiCodTitApIni fiCodTitApFim fiCodEspIni fiCodEspFim 
         fiTipoFluxoOrig fiTipoFluxoNovo btExec rt-button RECT-1 brTits RECT-2 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTitulosTpFluxo w-livre 
PROCEDURE getTitulosTpFluxo :
/*------------------------------------------------------------------------------
 - busca todos os titulos que tem na tabela val_tit_ap o tipo de fluxo
 original e que esteja dentro dos filtros passados
------------------------------------------------------------------------------*/
DEFINE VARIABLE dVlAcum AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cAcao   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE rRowid  AS ROWID       NO-UNDO.
DEFINE VARIABLE rRowidExcluir AS ROWID       NO-UNDO.
FOR EACH tit_ap NO-LOCK
    WHERE tit_ap.cod_estab          >= fiCodEstabIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    AND   tit_ap.cod_estab          <= fiCodEstabFim:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
    AND   tit_ap.dat_emis           >= INPUT FRAME {&FRAME-NAME} fiDtEmisIni
    AND   tit_ap.dat_emis           <= INPUT FRAME {&FRAME-NAME} fiDtEmisFim 
    AND   tit_ap.cdn_fornec         >= int(fiCodEmitIni:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    AND   tit_ap.cdn_fornec         <= int(fiCodEmitFim:SCREEN-VALUE IN FRAME {&FRAME-NAME})
    AND   tit_ap.cod_tit_ap         >= fiCodtitApIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    AND   tit_ap.cod_tit_ap         <= fiCodTitapFim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    AND   tit_ap.cod_espec_docto    >= fiCodEspIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    AND   tit_ap.cod_espec_docto    <= fiCodEspFim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    USE-INDEX titap_emis.   

    //IMPLANTACAO
    ASSIGN dVlAcum = 0
           rROWID = ?
           rRowidExcluir = ?
           cAcao = ''.

    FIND FIRST val_tit_ap OF tit_ap NO-LOCK
        WHERE val_tit_ap.cod_tip_fluxo_financ = fiTipoFluxoNovo:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
        NO-ERROR.
    IF AVAIL val_tit_ap THEN DO:
       ASSIGN dVlAcum   = val_tit_ap.val_origin_tit_ap
              rRowid    = ROWID(val_tit_ap).
    END.                                             
    FIND FIRST val_tit_ap OF tit_ap NO-LOCK
        WHERE val_tit_ap.cod_tip_fluxo_financ = fiTipoFluxoOrig:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
        NO-ERROR.
    IF AVAIL val_tit_ap  THEN DO:
        /*MESSAGE 'achei no val_tit_ap'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       IF dvlAcum = 0 THEN
          ASSIGN cAcao = 'subst'
                 rRowid = ROWID(val_tit_ap).
       ELSE
          ASSIGN cAcao = 'acum'
                rRowidExcluir = ROWID(val_tit_ap).
       ASSIGN  dVlAcum = IF dvlAcum > 0 THEN dvlAcum  + val_tit_ap.val_origin_tit_ap ELSE 0 .
       RUN inserirTTTitulo(ROWID(tit_ap),'IMPL',cAcao,dVlAcum,rRowid, rrowidExcluir ).
    END.         

    //MOVTO
    
    FOR EACH movto_tit_ap OF tit_ap NO-LOCK.
        ASSIGN dVlAcum = 0.
        FIND FIRST val_movto_ap OF movto_tit_ap 
            WHERE val_movto_ap.cod_tip_fluxo_financ = fiTipoFluxoNovo:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
            NO-LOCK NO-ERROR.
        IF AVAIL val_movto_ap THEN DO:
           ASSIGN dVlAcum   = val_movto_ap.val_pagto_tit_ap
                  rRowid    = ROWID(val_movto_ap) .
        END.


        FIND FIRST val_movto_ap OF movto_tit_ap 
            WHERE val_movto_ap.cod_tip_fluxo_financ = fiTipoFluxoOrig:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
            NO-LOCK NO-ERROR.
        IF AVAIL val_movto_ap THEN DO:
           /*MESSAGE 'achei no movimento'
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           IF dvlAcum = 0 THEN
              ASSIGN cAcao = 'subst'
                     rRowid = ROWID(val_movto_ap).
           ELSE
              ASSIGN cAcao = 'acum'
                     rRowidExcluir = ROWID(val_movto_ap).
          // caso nÆo tenha valor acumulado, nÆo tem necessidade de ajutar o valor. Apenas ser  necess rio alterar o tipo de fluxo
           ASSIGN  dVlAcum = IF dvlAcum > 0 THEN dVlAcum + val_movto_ap.val_pagto_tit_ap ELSE 0.
           RUN inserirTTTitulo(ROWID(tit_ap),'MOVTO',cAcao,dVlAcum,rRowid,rRowidExcluir  ).  
        END.
    END.    
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inserirTTTitulo w-livre 
PROCEDURE inserirTTTitulo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  (ROWID(tit_ap),'IMPL',cAcao,dVlAcum,rRowid )
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pRowidTitulo AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pOrigem      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pAcao        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pVlAcum      AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pRowidAjuste AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pRowidExcluir AS ROWID       NO-UNDO.

FIND tit_ap NO-LOCK 
    WHERE rowid(tit_ap) = pRowidTitulo
    NO-ERROR.



CREATE ttTitulos .
ASSIGN 
ttTitulos.rRowid          = pRowidTitulo
ttTitulos.codEstab        = IF AVAIL tit_ap THEN tit_ap.cod_estab           ELSE ''
ttTitulos.codSerie        = IF AVAIL tit_ap THEN tit_ap.cod_ser_docto       ELSE ''
ttTitulos.codEspec        = IF AVAIL tit_ap THEN tit_ap.cod_espec_docto     ELSE ''
ttTitulos.codTitAp        = IF AVAIL tit_ap THEN tit_ap.cod_tit_ap          ELSE ''
ttTitulos.codFornec       = IF AVAIL tit_ap THEN tit_ap.cdn_fornec          ELSE 0
ttTitulos.codParcela      = IF AVAIL tit_ap THEN tit_ap.cod_parcela         ELSE ''
ttTitulos.valor           = IF AVAIL tit_ap THEN tit_ap.val_origin_tit_ap   ELSE 0
ttTitulos.acao            = pAcao
ttTitulos.origem          = pOrigem
ttTitulos.vlAjuste        = pVlAcum
ttTitulos.rRowidAjuste    = pRowidAjuste     
ttTitulos.rRowidExcluir   = pRowidExcluir
 .








                                              
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

  {utp/ut9000.i "ESFIN010" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fiDtEmisIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(TODAY - 90)
         fiDtEmisFim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(TODAY)
      .
         

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttTitulos"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAlteracoes w-livre 
PROCEDURE setAlteracoes :
/*------------------------------------------------------------------------------

------------------------------------------------------------------------------*/
DEFINE BUFFER bfValTitAp   FOR  val_tit_ap .
DEFINE BUFFER bfValMovtoAp FOR  val_movto_ap .
/*OUTPUT TO c:\temp\ttTitulos.txt.
FOR EACH ttTitulos:
    ttTitulos.
END.
OUTPUT CLOSE.
*/

FOR EACH ttTitulos:
    CASE ttTitulos.origem:
        WHEN 'impl' THEN DO:
            /*MESSAGE 'impl'
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            FIND val_tit_ap
                WHERE ROWID(val_tit_ap) = ttTitulos.rRowidAjuste
                NO-ERROR.
            IF AVAIL val_tit_ap THEN DO:
                /*MESSAGE 'achei o titulo' SKIP
                        ttTitulos.vlAjuste
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
               IF ttTitulos.vlAjuste > 0 THEN DO:
                  /*MESSAGE 'reajuste'
                      'valor atual do titulo:' val_tit_ap.val_pagto_tit_ap
                      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                  ASSIGN val_tit_ap.val_pagto_tit_ap  = ttTitulos.vlAjuste
                         val_tit_ap.val_origin_tit_ap = ttTitulos.vlAjuste .
                  /*MESSAGE 'novo valor do titulo:' val_tit_ap.val_pagto_tit_ap
                         'rowid excluir' STRING(ttTitulos.rRowidExcluir)
                      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

                  FIND bfValTitAp
                      WHERE rowid(bfvalTitap) = ttTitulos.rRowidExcluir
                      EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAIL bfValTitAp THEN DO:
                     DELETE bfValTitap.
                     /*MESSAGE 'deletei registro'
                         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                  END.
                     
               END.
               ELSE DO:
                  /*MESSAGE 'alteracao fluxo'
                      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                  ASSIGN val_tit_ap.cod_tip_fluxo_financ = fitipoFluxoNovo:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
               END.
                  
            END.  
        END.
        WHEN 'movto' THEN DO:
            /*MESSAGE 'entrei movto'
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            FIND val_movto_ap
                WHERE ROWID(val_movto_ap) = ttTitulos.rRowidAjuste
                NO-ERROR.
            IF AVAIL val_movto_ap THEN DO:
               /*MESSAGE 'achei movto'
                   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
               IF ttTitulos.vlAjuste > 0 THEN DO:
                   /*MESSAGE 'ajuste '
                       'vl.anterior:' val_movto_ap.val_pagto_tit_ap SKIP
                       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

                   ASSIGN val_movto_ap.val_pagto_tit_ap = ttTitulos.vlAjuste.

                   /*MESSAGE 'vl.posterior:' val_movto_ap.val_pagto_tit_ap SKIP
                       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

                   FIND bfValMovtoAp
                       WHERE rowid(bfvalMovtoap) = ttTitulos.rRowidExcluir
                       EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAIL bfValMovtoAp THEN DO:
                      DELETE bfValMovtoap.
                      /*MESSAGE 'deletou'
                          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                   END.
                      

               END.
               ELSE
                  ASSIGN val_movto_ap.cod_tip_fluxo_financ = fitipoFluxoNovo:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
            END.
        END.
    END CASE.
END.

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

