&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems5             PROGRESS
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

DEFINE VARIABLE cCNPJ AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE VARIABLE iPessoa AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ttCliente
       FIELD cod_cliente AS INT
       FIELD nome_cliente AS CHAR FORMAT 'x(20)'.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br_cliente

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCliente pessoa_fisic pessoa_jurid

/* Definitions for BROWSE br_cliente                                    */
&Scoped-define FIELDS-IN-QUERY-br_cliente ttCliente.cod_cliente ttCliente.nome_cliente   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_cliente   
&Scoped-define SELF-NAME br_cliente
&Scoped-define QUERY-STRING-br_cliente FOR EACH ttCliente
&Scoped-define OPEN-QUERY-br_cliente OPEN QUERY {&SELF-NAME} FOR EACH ttCliente.
&Scoped-define TABLES-IN-QUERY-br_cliente ttCliente
&Scoped-define FIRST-TABLE-IN-QUERY-br_cliente ttCliente


/* Definitions for BROWSE br_pessoa_fisica                              */
&Scoped-define FIELDS-IN-QUERY-br_pessoa_fisica ~
pessoa_fisic.num_pessoa_fisic pessoa_fisic.nom_pessoa 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_pessoa_fisica 
&Scoped-define QUERY-STRING-br_pessoa_fisica FOR EACH pessoa_fisic ~
      WHERE pessoa_fisic.num_pessoa_fisic = iPessoa NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_pessoa_fisica OPEN QUERY br_pessoa_fisica FOR EACH pessoa_fisic ~
      WHERE pessoa_fisic.num_pessoa_fisic = iPessoa NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_pessoa_fisica pessoa_fisic
&Scoped-define FIRST-TABLE-IN-QUERY-br_pessoa_fisica pessoa_fisic


/* Definitions for BROWSE br_pessoa_Juridica                            */
&Scoped-define FIELDS-IN-QUERY-br_pessoa_Juridica ~
pessoa_jurid.num_pessoa_jurid pessoa_jurid.num_pessoa_jurid_matriz ~
pessoa_jurid.nom_pessoa pessoa_jurid.cod_id_feder 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_pessoa_Juridica 
&Scoped-define QUERY-STRING-br_pessoa_Juridica FOR EACH pessoa_jurid ~
      WHERE pessoa_jurid.cod_id_feder = cCnpj NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_pessoa_Juridica OPEN QUERY br_pessoa_Juridica FOR EACH pessoa_jurid ~
      WHERE pessoa_jurid.cod_id_feder = cCnpj NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_pessoa_Juridica pessoa_jurid
&Scoped-define FIRST-TABLE-IN-QUERY-br_pessoa_Juridica pessoa_jurid


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br_pessoa_fisica}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button fi_cod_cliente bt_buscar ~
br_pessoa_Juridica br_cliente br_pessoa_fisica btDeletarPJ btDeletarPF ~
btAssociar 
&Scoped-Define DISPLAYED-OBJECTS fi_cod_cliente 

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
DEFINE BUTTON btAssociar 
     LABEL "Associar PJ" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btDeletarPF 
     LABEL "Deletar PF" 
     SIZE 15 BY 1.13.

DEFINE BUTTON btDeletarPJ 
     LABEL "Deletar PJ" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt_buscar 
     LABEL "Buscar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fi_cod_cliente AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 112 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_cliente FOR 
      ttCliente SCROLLING.

DEFINE QUERY br_pessoa_fisica FOR 
      pessoa_fisic SCROLLING.

DEFINE QUERY br_pessoa_Juridica FOR 
      pessoa_jurid SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_cliente w-livre _FREEFORM
  QUERY br_cliente DISPLAY
      ttCliente.cod_cliente  COLUMN-LABEL "c¢digo"
 ttCliente.nome_cliente COLUMN-LABEL "nome"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 4.5
         FONT 1
         TITLE "Clientes - Pessoa Juridica Corrente" FIT-LAST-COLUMN.

DEFINE BROWSE br_pessoa_fisica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_pessoa_fisica w-livre _STRUCTURED
  QUERY br_pessoa_fisica NO-LOCK DISPLAY
      pessoa_fisic.num_pessoa_fisic FORMAT ">>>,>>>,>>9":U
      pessoa_fisic.nom_pessoa FORMAT "x(40)":U WIDTH 88.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105.14 BY 4.5
         FONT 1
         TITLE "Pessoa Fisica Associada ao Cliente" FIT-LAST-COLUMN.

DEFINE BROWSE br_pessoa_Juridica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_pessoa_Juridica w-livre _STRUCTURED
  QUERY br_pessoa_Juridica NO-LOCK DISPLAY
      pessoa_jurid.num_pessoa_jurid FORMAT ">>>,>>>,>>9":U
      pessoa_jurid.num_pessoa_jurid_matriz FORMAT ">>>,>>>,>>9":U
      pessoa_jurid.nom_pessoa FORMAT "x(40)":U WIDTH 46.29
      pessoa_jurid.cod_id_feder FORMAT "x(20)":U WIDTH 28.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 106 BY 5
         FONT 1
         TITLE "Pessoas Juridicas" ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi_cod_cliente AT ROW 3 COL 11 COLON-ALIGNED WIDGET-ID 2
     bt_buscar AT ROW 3 COL 29 WIDGET-ID 4
     br_pessoa_Juridica AT ROW 4.5 COL 2 WIDGET-ID 200
     br_cliente AT ROW 10 COL 2 WIDGET-ID 300
     br_pessoa_fisica AT ROW 15.42 COL 2.86 WIDGET-ID 400
     btDeletarPJ AT ROW 20.25 COL 3 WIDGET-ID 8
     btDeletarPF AT ROW 20.25 COL 18 WIDGET-ID 10
     btAssociar AT ROW 20.25 COL 33 WIDGET-ID 12
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 21.54
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
         HEIGHT             = 21.54
         WIDTH              = 112
         MAX-HEIGHT         = 26.83
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 26.83
         VIRTUAL-WIDTH      = 114
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
/* BROWSE-TAB br_pessoa_Juridica bt_buscar f-cad */
/* BROWSE-TAB br_cliente br_pessoa_Juridica f-cad */
/* BROWSE-TAB br_pessoa_fisica br_cliente f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_cliente
/* Query rebuild information for BROWSE br_cliente
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCliente.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE br_cliente */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_pessoa_fisica
/* Query rebuild information for BROWSE br_pessoa_fisica
     _TblList          = "ems5.pessoa_fisic"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5.pessoa_fisic.num_pessoa_fisic = iPessoa"
     _FldNameList[1]   = ems5.pessoa_fisic.num_pessoa_fisic
     _FldNameList[2]   > ems5.pessoa_fisic.nom_pessoa
"nom_pessoa" ? ? "character" ? ? ? ? ? ? no ? no no "88.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br_pessoa_fisica */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_pessoa_Juridica
/* Query rebuild information for BROWSE br_pessoa_Juridica
     _TblList          = "ems5.pessoa_jurid"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems5.pessoa_jurid.cod_id_feder = cCnpj"
     _FldNameList[1]   = ems5.pessoa_jurid.num_pessoa_jurid
     _FldNameList[2]   = ems5.pessoa_jurid.num_pessoa_jurid_matriz
     _FldNameList[3]   > ems5.pessoa_jurid.nom_pessoa
"pessoa_jurid.nom_pessoa" ? ? "character" ? ? ? ? ? ? no ? no no "46.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ems5.pessoa_jurid.cod_id_feder
"pessoa_jurid.cod_id_feder" ? ? "character" ? ? ? ? ? ? no ? no no "28.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_pessoa_Juridica */
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


&Scoped-define BROWSE-NAME br_pessoa_Juridica
&Scoped-define SELF-NAME br_pessoa_Juridica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_pessoa_Juridica w-livre
ON VALUE-CHANGED OF br_pessoa_Juridica IN FRAME f-cad /* Pessoas Juridicas */
DO:
    EMPTY TEMP-TABLE ttCliente.
    ASSIGN iCont = 0.
    FOR EACH ems5.cliente NO-LOCK
        WHERE cliente.num_pessoa = pessoa_jurid.num_pessoa_jurid .
        CREATE ttCliente.
        ASSIGN ttCliente.cod_cliente  = cliente.cdn_cliente
               ttCliente.nome_cliente = cliente.nom_pessoa.
        ASSIGN iCont = iCont + 1.
    END.
    {&OPEN-QUERY-br_cliente}
    

    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAssociar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAssociar w-livre
ON CHOOSE OF btAssociar IN FRAME f-cad /* Associar PJ */
DO:
  DISABLE TRIGGERS FOR LOAD OF ems5.cliente.
  FIND FIRST ems5.cliente EXCLUSIVE-LOCK
      WHERE cliente.cdn_cliente = INPUT FRAME {&FRAME-NAME} fi_cod_cliente.
  IF AVAIL ems5.cliente THEN DO:
     ASSIGN ems5.cliente.num_pessoa = pessoa_jurid.num_pessoa_jurid
            cliente.cod_id_feder = pessoa_jurid.cod_id_feder.
  END.
  ELSE DO:
      MESSAGE 'cliente n∆o encontrado'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  RELEASE ems5.cliente.
  {&OPEN-QUERY-br_pessoa_fisica}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDeletarPF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeletarPF w-livre
ON CHOOSE OF btDeletarPF IN FRAME f-cad /* Deletar PF */
DO:
  DISABLE TRIGGERS FOR LOAD OF pessoa_fisic.
  FIND CURRENT pessoa_fisic EXCLUSIVE-LOCK.
  DELETE pessoa_fisic.
  RELEASE pessoa_fisic.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDeletarPJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeletarPJ w-livre
ON CHOOSE OF btDeletarPJ IN FRAME f-cad /* Deletar PJ */
DO:
  IF iCont > 0 THEN DO:
     MESSAGE ' A pessoa juridica tem ligaá∆o com um ou mais clientes e por isto n∆o pode ser deletada'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE DO:
     FIND CURRENT pessoa_jurid EXCLUSIVE-LOCK.
     DELETE pessoa_jurid.
     RELEASE pessoa_jurid.
     {&OPEN-QUERY-br_pessoa_Juridica}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_buscar w-livre
ON CHOOSE OF bt_buscar IN FRAME f-cad /* Buscar */
DO:
  FIND FIRST emitente
      WHERE emitente.cod-emitente = INPUT FRAME {&frame-name} fi_cod_cliente NO-LOCK NO-ERROR.
  IF AVAIL emitente THEN DO:
     ASSIGN cCnpj = emitente.cgc.
     {&OPEN-QUERY-br_pessoa_Juridica}
     FIND FIRST ems5.cliente
         WHERE cliente.cdn_cliente  = emitente.cod-emitente NO-LOCK NO-ERROR.
     IF AVAIL ems5.cliente THEN DO:
        ASSIGN iPessoa = ems5.cliente.num_pessoa .
        {&OPEN-QUERY-br_pessoa_fisica}
     END.
       
  END.
  ELSE DO:
     MESSAGE 'cliente n∆o encontrado'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
  END.               



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


&Scoped-define BROWSE-NAME br_cliente
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
       RUN set-position IN h_p-exihel ( 1.13 , 96.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fi_cod_cliente:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fi_cod_cliente 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fi_cod_cliente bt_buscar br_pessoa_Juridica br_cliente 
         br_pessoa_fisica btDeletarPJ btDeletarPF btAssociar 
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

  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "pessoa_jurid"}
  {src/adm/template/snd-list.i "ttCliente"}
  {src/adm/template/snd-list.i "pessoa_fisic"}

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

