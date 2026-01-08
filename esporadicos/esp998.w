&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i esp998 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
DEF TEMP-TABLE tt_tit_acr           LIKE ems5.tit_acr.
DEF TEMP-TABLE tt_aprop_ctbl_acr LIKE ems5.aprop_ctbl_acr.
DEFINE VARIABLE qtDias AS INTEGER     NO-UNDO.
DEFINE VARIABLE cErros AS CHARACTER   NO-UNDO.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE data_corrente AS DATE        NO-UNDO.
DEFINE VARIABLE i             AS INTEGER     NO-UNDO.
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lAchouTit AS LOGICAL     NO-UNDO.

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.
DEFINE VARIABLE iLinha AS INTEGER     NO-UNDO.

DEFINE VARIABLE lIntegrou AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button bt_atualizar fi_data_ini ~
fi_data_fim 
&Scoped-Define DISPLAYED-OBJECTS fi_data_ini fi_data_fim 

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
DEFINE BUTTON bt_atualizar 
     LABEL "Atualizar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fi_data_fim AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi_data_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt_atualizar AT ROW 3.25 COL 59 WIDGET-ID 2
     fi_data_ini AT ROW 3.33 COL 10.14 COLON-ALIGNED WIDGET-ID 4
     fi_data_fim AT ROW 3.33 COL 39 COLON-ALIGNED WIDGET-ID 6
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
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
         TITLE              = "Atualiza‡Æo dos t¡tutlos do BKP"
         HEIGHT             = 4.63
         WIDTH              = 89.72
         MAX-HEIGHT         = 33
         MAX-WIDTH          = 228.57
         VIRTUAL-HEIGHT     = 33
         VIRTUAL-WIDTH      = 228.57
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Atualiza‡Æo dos t¡tutlos do BKP */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Atualiza‡Æo dos t¡tutlos do BKP */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_atualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_atualizar w-livre
ON CHOOSE OF bt_atualizar IN FRAME f-cad /* Atualizar */
DO:
  
RUN utp/ut-acomp.p PERSIST SET h-acomp.
RUN pi-inicializar IN h-acomp('Integrando Titulos BKP').

IF CONNECTED("db-aux") THEN
   DISCONNECT db-aux NO-ERROR.

IF CONNECTED("db-aux2") THEN
   DISCONNECT db-aux2 NO-ERROR.


CONNECT -db ems5    -ld db-aux  -S 10032 -H 192.168.0.111 -N tcp NO-ERROR.
CONNECT -db ems2med -ld db-aux2 -S 10031 -H 192.168.0.111 -N tcp NO-ERROR.



ASSIGN cArquivo = "p:\atualiza_titulos_bkp" + STRING(TIME) + ".csv".
OUTPUT TO VALUE(cArquivo) NO-CONVERT.
ASSIGN qtDias = INT(INPUT FRAME {&frame-name} fi_data_fim - INPUT FRAME {&frame-name} fi_data_ini) + 1 .
ASSIGN data_corrente = INPUT FRAME {&frame-name} fi_data_ini.
//DISP "................Listagem Dias Processados....:" SKIP.
PUT "Empresa;Estab;Cliente;Espec;Titulo;Refer;Dt.Trans.;Parcela;Vl.Original;Portador;J  Existe?;Log.Integra‡Æo" SKIP.
IF qtDias = 0 THEN DO:
   EMPTY TEMP-TABLE tt_tit_acr.
   EMPTY TEMP-TABLE tt_aprop_ctbl_acr.
   //DISP "Data:"  data_corrente SKIP.
   RUN pi-acompanhar IN h-acomp('buscando titulos do dia').
   RUN esp-aux/busca-titulos.p (data_corrente,
                                OUTPUT TABLE tt_tit_acr,
                                OUTPUT TABLE tt_aprop_ctbl_acr
                                ). 
END.
ELSE DO:
  ASSIGN iLinha = 0.
  REPEAT i = 1 TO qtDias:
     RUN pi-acompanhar IN h-acomp('buscando titulos - data:' + STRING(data_corrente)).
    //DISP "Data:"  data_corrente SKIP.
    ASSIGN iLinha = iLinha + 1.

    EMPTY TEMP-TABLE tt_tit_acr.
    EMPTY TEMP-TABLE tt_aprop_ctbl_acr.
    RUN esp-aux/busca-titulos.p (data_corrente,
                                 OUTPUT TABLE tt_tit_acr,
                                 OUTPUT TABLE tt_aprop_ctbl_acr). 
   

    ASSIGN data_corrente = data_corrente + 1.
  
    FOR EACH tt_tit_acr .
    

      ASSIGN lIntegrou = NO.
      FIND   ems5.tit_acr WHERE
             tit_acr.cod_estab        = tt_tit_acr.cod_estab        AND
             tit_acr.cod_espec_docto  = tt_tit_acr.cod_espec_docto  AND
             tit_acr.cod_ser_docto    = tt_tit_acr.cod_ser_docto    AND 
             tit_acr.cod_tit_acr      = tt_tit_acr.cod_tit_acr      AND   
             tit_acr.cod_parcela      = tt_tit_acr.cod_parcela 
             NO-LOCK NO-ERROR.
       ASSIGN lAchouTit =   AVAIL tit_acr.   
                      
       IF NOT lAchouTit THEN DO:
          ASSIGN lIntegrou = YES.
          RUN esp-aux\importa_tit_acr.p (INPUT TABLE tt_tit_acr,
                                         INPUT TABLE tt_aprop_ctbl_acr,
                                         OUTPUT cErros).
       END.
       ELSE 
         ASSIGN cErros = "Titulo j  Existente na Base BKP".
       
       IF cErros = '' THEN
          ASSIGN cErros = "Titulo Incluido Com Sucesso".

       //integra o cliente e tenta integrar novamente o titulo  
       IF lIntegrou = YES AND substr(cErros,1,4) = '7258' THEN DO:
          RUN esporadicos\integrarClienteBKPManual.p(tt_tit_acr.cdn_cliente).
          RUN esp-aux\importa_tit_acr.p (INPUT TABLE tt_tit_acr,
                                         INPUT TABLE tt_aprop_ctbl_acr,
                                         OUTPUT cErros).
          IF cErros = '' THEN
             ASSIGN cErros = "Titulo Incluido Com Sucesso - Foi necess rio integrar o cliente no BKP".


       END.

       
       EXPORT DELIMITER ";"
       tt_tit_acr.cod_empresa
       tt_tit_acr.cod_estab
       tt_tit_acr.cdn_cliente
       tt_tit_acr.cod_espec_docto
       tt_tit_acr.cod_tit_acr
       tt_tit_acr.cod_refer
       tt_tit_acr.dat_transacao
       tt_tit_acr.cod_parcela
       tt_tit_acr.val_origin_tit_acr
       tt_tit_acr.cod_portador
       lAchouTit FORMAT 'Sim/NÆo'
       cErros
       .
    END.
   END.
END.
OUTPUT CLOSE.
DISCONNECT db-aux NO-ERROR.
DISCONNECT db-aux2 NO-ERROR.    

//DISP "................Final da Listagem.........." SKIP.
RUN pi-finalizar IN h-acomp.
MESSAGE "Gerado arquivo Para Conferˆncia da Integra‡Æo:" SKIP cArquivo
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
OS-COMMAND SILENT VALUE(' start ' + cArquivo) NO-WAIT.
/*MESSAGE 'Processamento Finalizado'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
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
             bt_atualizar:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fi_data_ini fi_data_fim 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button bt_atualizar fi_data_ini fi_data_fim 
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

  {utp/ut9000.i "esp998" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi_data_ini:SCREEN-VALUE = STRING(TODAY)
         fi_data_fim:SCREEN-VALUE = STRING(TODAY)
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

