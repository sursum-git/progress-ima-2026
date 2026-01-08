&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
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
{include/i-prgvrs.i ESSP0111 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR c-nome-abrev  AS CHAR FORMAT "x(12)".
DEF VAR c-situacao    AS CHAR FORMAT "x(12)".
DEF VAR c-nr-pedcli   AS CHAR FORMAT "x(12)".
DEF VAR i-nr-nota-fis AS INT  FORMAT ">>>>,>>9".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ob-etiqueta

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 ob-etiqueta.nr-sequencia ~
ob-etiqueta.nr-carro ob-etiqueta.num-etiqueta ob-etiqueta.dt-emissao ~
ob-etiqueta.hr-emissao ob-etiqueta.nr-lote ob-etiqueta.quantidade ~
fn-situacao() @ c-situacao ob-etiqueta.localizacao ob-etiqueta.nr-reporte ~
ob-etiqueta.resp-revisao fn-nome-abrev() @ c-nome-abrev ~
fn-nr-pedcli() @ c-nr-pedcli fn-nr-nota-fis() @ i-nr-nota-fis 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH ob-etiqueta ~
      WHERE ob-etiqueta.nr-ob = fi-nr-ob NO-LOCK ~
    BY ob-etiqueta.nr-sequencia INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH ob-etiqueta ~
      WHERE ob-etiqueta.nr-ob = fi-nr-ob NO-LOCK ~
    BY ob-etiqueta.nr-sequencia INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 ob-etiqueta


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt-ant bt-prox btn-vapra fi-nr-ob ~
BROWSE-1 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-ob fi-cod-estabel fi-nome-estabel ~
fi-it-codigo fi-desc-item fi-cod-refer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-nome-abrev w-livre 
FUNCTION fn-nome-abrev RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-nr-nota-fis w-livre 
FUNCTION fn-nr-nota-fis RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-nr-pedcli w-livre 
FUNCTION fn-nr-pedcli RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-situacao w-livre 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
DEFINE BUTTON bt-ant 
     IMAGE-UP FILE "image/im-ante.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Consulta Etiqueta Anterior".

DEFINE BUTTON bt-prox 
     IMAGE-UP FILE "image/im-nex.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1 TOOLTIP "Consulta Proxima Etiqueta".

DEFINE BUTTON btn-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Btn 4" 
     SIZE 4 BY 1.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "x(3)" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 115 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 w-livre _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      ob-etiqueta.nr-sequencia COLUMN-LABEL "Seq" FORMAT ">>9":U
            WIDTH 4
      ob-etiqueta.nr-carro FORMAT "9XX":U
      ob-etiqueta.num-etiqueta FORMAT "999999999":U
      ob-etiqueta.dt-emissao COLUMN-LABEL "Emiss∆o" FORMAT "99/99/9999":U
            WIDTH 10
      ob-etiqueta.hr-emissao COLUMN-LABEL "Hora" FORMAT "x(5)":U
            WIDTH 6
      ob-etiqueta.nr-lote FORMAT "X(8)":U WIDTH 2.86
      ob-etiqueta.quantidade FORMAT ">>>,>>9.99":U WIDTH 8.57
      fn-situacao() @ c-situacao COLUMN-LABEL "Situaá∆o" WIDTH 9
      ob-etiqueta.localizacao FORMAT "XXX/XXX":U WIDTH 7
      ob-etiqueta.nr-reporte FORMAT ">>>>>>>>9":U WIDTH 6.43
      ob-etiqueta.resp-revisao COLUMN-LABEL "Resp.Rev." FORMAT "X(12)":U
            WIDTH 9.72
      fn-nome-abrev() @ c-nome-abrev COLUMN-LABEL "Cliente"
      fn-nr-pedcli() @ c-nr-pedcli COLUMN-LABEL "Pedido"
      fn-nr-nota-fis() @ i-nr-nota-fis COLUMN-LABEL "Nota Fiscal"
            WIDTH 15.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 114 BY 9
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-ant AT ROW 2.71 COL 30.43 WIDGET-ID 6
     bt-prox AT ROW 2.71 COL 34.43 WIDGET-ID 8
     btn-vapra AT ROW 2.75 COL 25.43
     fi-nr-ob AT ROW 2.79 COL 14 COLON-ALIGNED
     fi-cod-estabel AT ROW 3.79 COL 14 COLON-ALIGNED WIDGET-ID 2
     fi-nome-estabel AT ROW 3.79 COL 17.29 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-it-codigo AT ROW 4.79 COL 14 COLON-ALIGNED
     fi-desc-item AT ROW 4.79 COL 23.29 COLON-ALIGNED NO-LABEL
     fi-cod-refer AT ROW 5.71 COL 14 COLON-ALIGNED
     BROWSE-1 AT ROW 6.83 COL 2
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.86 BY 15.08
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
         TITLE              = "Consulta Etiquetas de uma OB"
         HEIGHT             = 14.92
         WIDTH              = 115.86
         MAX-HEIGHT         = 27.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.79
         VIRTUAL-WIDTH      = 146.29
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
/* BROWSE-TAB BROWSE-1 fi-cod-refer f-cad */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "espec.ob-etiqueta"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "espec.ob-etiqueta.nr-sequencia|yes"
     _Where[1]         = "espec.ob-etiqueta.nr-ob = fi-nr-ob"
     _FldNameList[1]   > espec.ob-etiqueta.nr-sequencia
"ob-etiqueta.nr-sequencia" "Seq" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = espec.ob-etiqueta.nr-carro
     _FldNameList[3]   = espec.ob-etiqueta.num-etiqueta
     _FldNameList[4]   > espec.ob-etiqueta.dt-emissao
"ob-etiqueta.dt-emissao" "Emiss∆o" ? "date" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > espec.ob-etiqueta.hr-emissao
"ob-etiqueta.hr-emissao" "Hora" "x(5)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > espec.ob-etiqueta.nr-lote
"ob-etiqueta.nr-lote" ? ? "character" ? ? ? ? ? ? no ? no no "2.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > espec.ob-etiqueta.quantidade
"ob-etiqueta.quantidade" ? ? "decimal" ? ? ? ? ? ? no ? no no "8.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"fn-situacao() @ c-situacao" "Situaá∆o" ? ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > espec.ob-etiqueta.localizacao
"ob-etiqueta.localizacao" ? "XXX/XXX" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > espec.ob-etiqueta.nr-reporte
"ob-etiqueta.nr-reporte" ? ? "integer" ? ? ? ? ? ? no ? no no "6.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > espec.ob-etiqueta.resp-revisao
"ob-etiqueta.resp-revisao" "Resp.Rev." ? "character" ? ? ? ? ? ? no ? no no "9.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"fn-nome-abrev() @ c-nome-abrev" "Cliente" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"fn-nr-pedcli() @ c-nr-pedcli" "Pedido" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"fn-nr-nota-fis() @ i-nr-nota-fis" "Nota Fiscal" ? ? ? ? ? ? ? ? no ? no no "15.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Consulta Etiquetas de uma OB */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Consulta Etiquetas de uma OB */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ant w-livre
ON CHOOSE OF bt-ant IN FRAME f-cad /* Button 2 */
DO:
  FIND PREV ordem-benefic NO-LOCK NO-ERROR.
  IF AVAIL ordem-benefic THEN DO.
     ASSIGN fi-nr-ob:SCREEN-VALUE = STRING(ordem-benefic.nr-ob).
     APPLY 'choose' TO btn-vapra.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prox w-livre
ON CHOOSE OF bt-prox IN FRAME f-cad /* Button 3 */
DO:
   FIND NEXT ordem-benefic NO-LOCK NO-ERROR.
   IF AVAIL ordem-benefic THEN DO.
      ASSIGN fi-nr-ob:SCREEN-VALUE = STRING(ordem-benefic.nr-ob).
      APPLY 'choose' TO btn-vapra.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-vapra w-livre
ON CHOOSE OF btn-vapra IN FRAME f-cad /* Btn 4 */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-ob.

  FIND FIRST ordem-benefic WHERE
             ordem-benefic.nr-ob = fi-nr-ob NO-LOCK NO-ERROR.

  IF AVAIL ordem-benefic THEN DO.
     ASSIGN fi-desc-item:FGCOLOR = ?
            fi-desc-item:FONT = ?.
     FIND item WHERE
          item.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.

     IF AVAIL ITEM THEN
        ASSIGN fi-desc-item:SCREEN-VALUE = ITEM.desc-item.

     ASSIGN fi-it-codigo:SCREEN-VALUE = ordem-benefic.it-codigo
            fi-cod-refer:SCREEN-VALUE = ordem-benefic.cod-refer.

     FIND estabelec WHERE
          estabelec.cod-estabel = ordem-benefic.cod-estabel NO-LOCK NO-ERROR.
     IF AVAIL estabelec THEN
        ASSIGN fi-cod-estabel:SCREEN-VALUE = ordem-benefic.cod-estabel
               fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

  END.
  ELSE DO.
      ASSIGN fi-desc-item:FGCOLOR = 12
             fi-desc-item:FONT = 9.
      ASSIGN fi-desc-item:SCREEN-VALUE = "ERRO: OB n∆o encontrada no Sistema...".

      ASSIGN fi-it-codigo:SCREEN-VALUE = ''
             fi-cod-refer:SCREEN-VALUE = ''.
  END.

  {&OPEN-QUERY-BROWSE-1}

   APPLY 'entry' TO fi-nr-ob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-ob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-ob w-livre
ON RETURN OF fi-nr-ob IN FRAME f-cad /* N£mero da OB */
DO:
  APPLY 'choose' TO btn-vapra.
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


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

APPLY 'choose' TO btn-vapra.

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
       RUN set-position IN h_p-exihel ( 1.17 , 99.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-ant:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fi-nr-ob fi-cod-estabel fi-nome-estabel fi-it-codigo fi-desc-item 
          fi-cod-refer 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button bt-ant bt-prox btn-vapra fi-nr-ob BROWSE-1 
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

  {utp/ut9000.i "ESSP0111" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND LAST ordem-benefic WHERE 
            ordem-benefic.cod-estabel <> "" NO-LOCK NO-ERROR.
  ASSIGN fi-nr-ob:SCREEN-VALUE = STRING(ordem-benefic.nr-ob).

  APPLY 'entry' TO fi-nr-ob.
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
  {src/adm/template/snd-list.i "ob-etiqueta"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nome-abrev w-livre 
FUNCTION fn-nome-abrev RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND ped-item-rom USE-INDEX indice3  WHERE                     
        ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta AND
        ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel
       NO-LOCK NO-ERROR.                    
                                                                    
   ASSIGN c-nome-abrev = "".          
   IF AVAIL ped-item-rom THEN                                        
      ASSIGN c-nome-abrev = ped-item-rom.nome-abrev.                
   RETURN c-nome-abrev.   /* Function return value. */               
                                                                    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nr-nota-fis w-livre 
FUNCTION fn-nr-nota-fis RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   FIND ped-item-res USE-INDEX INDICE1
       WHERE ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
             ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
             ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
       NO-LOCK NO-ERROR.
   ASSIGN i-nr-nota-fis = 0.
   IF AVAIL ped-item-res THEN                                        
      ASSIGN i-nr-nota-fis = ped-item-res.nr-nota-fis.                
                                                                    
   RETURN string(i-nr-nota-fis).   /* Function return value. */               
                                                                    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nr-pedcli w-livre 
FUNCTION fn-nr-pedcli RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND ped-item-rom USE-INDEX indice3 WHERE                  
        ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta AND
        ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel
        NO-LOCK NO-ERROR.                    
                                                                    
   ASSIGN c-nr-pedcli = "".          
   IF AVAIL ped-item-rom THEN                                        
      ASSIGN c-nr-pedcli = ped-item-rom.nr-pedcli.                
                                                                    
   RETURN c-nr-pedcli.   /* Function return value. */               
                                                                    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-situacao w-livre 
FUNCTION fn-situacao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

      {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 

  RETURN c-situacao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

