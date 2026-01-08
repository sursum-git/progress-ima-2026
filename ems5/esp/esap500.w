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

{esbo/bo_repres.i}
{utp/ut-glob.i}
{esbo/boAppServer.i}
DEFINE TEMP-TABLE ttAux LIKE ttDocs.
DEFINE VARIABLE hBoRepres           AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoRepresEmpresa    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cLista              AS CHARACTER FORMAT 'x(2000)'   NO-UNDO.
DEFINE VARIABLE iQtMeses            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtMesesIMA         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtMesesMED         AS INTEGER     NO-UNDO.
DEFINE VARIABLE dValorNota          AS DECIMAL  FORMAT '>>>,>>>,>>>.99'   NO-UNDO.
DEFINE VARIABLE dValorLiquido       AS DECIMAL  FORMAT '>>>,>>>,>>>.99'   NO-UNDO.
DEFINE VARIABLE dValorNotaIMA       AS DECIMAL  FORMAT '>>>,>>>,>>>.99'   NO-UNDO.
DEFINE VARIABLE dValorLiquidoIMA    AS DECIMAL  FORMAT '>>>,>>>,>>>.99'   NO-UNDO.
DEFINE VARIABLE dValorNotaMED       AS DECIMAL  FORMAT '>>>,>>>,>>>.99'   NO-UNDO.
DEFINE VARIABLE dValorLiquidoMED    AS DECIMAL  FORMAT '>>>,>>>,>>>.99'   NO-UNDO.
DEFINE VARIABLE cOutrosFornecs      AS CHARACTER   NO-UNDO FORMAT 'x(30)'.
DEFINE VARIABLE hboAppServer        AS HANDLE      NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS rt-button cb_repres fiFornecedores fi_dt_ini ~
fi_dt_fim btExecutar 
&Scoped-Define DISPLAYED-OBJECTS cb_repres fiFornecedores fi_dt_ini ~
fi_dt_fim 

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
DEFINE BUTTON btExecutar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE cb_repres AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Representante Externo" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEM-PAIRS "Carregando Representantes",0
     DROP-DOWN-LIST
     SIZE 49 BY 1 NO-UNDO.

DEFINE VARIABLE fiFornecedores AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fornecedores" 
     VIEW-AS FILL-IN 
     SIZE 49 BY .79 NO-UNDO.

DEFINE VARIABLE fi_dt_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .79 NO-UNDO.

DEFINE VARIABLE fi_dt_ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Dt.Emiss∆o De" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .79 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     cb_repres AT ROW 3.63 COL 20 COLON-ALIGNED WIDGET-ID 2
     fiFornecedores AT ROW 4.63 COL 20 COLON-ALIGNED WIDGET-ID 10
     fi_dt_ini AT ROW 5.63 COL 20 COLON-ALIGNED WIDGET-ID 6
     fi_dt_fim AT ROW 5.63 COL 37 COLON-ALIGNED WIDGET-ID 8
     btExecutar AT ROW 6.67 COL 22 WIDGET-ID 4
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.86 BY 9.92
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
         TITLE              = "Pagtos Representante"
         HEIGHT             = 8.13
         WIDTH              = 89.57
         MAX-HEIGHT         = 18.33
         MAX-WIDTH          = 125.14
         VIRTUAL-HEIGHT     = 18.33
         VIRTUAL-WIDTH      = 125.14
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
ON END-ERROR OF w-livre /* Pagtos Representante */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Pagtos Representante */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecutar w-livre
ON CHOOSE OF btExecutar IN FRAME f-cad /* Executar */
DO:
  
  EMPTY TEMP-TABLE ttAux.
  EMPTY TEMP-TABLE ttDocs.
  
  FOR EACH ttEmpresa:   
     
      RUN esbo/bo_repres.p PERSIST SET hBoRepresEmpresa ON SERVER ttEmpresa.appServer.
      RUN limparTTs             IN hBoRepresEmpresa.
      RUN setEmpresa            IN hBoRepresEmpresa(ttEmpresa.codEmpresa,ttEmpresa.codEmpresa).
      RUN setRepresentante      IN hBoRepresEmpresa(INPUT FRAME {&FRAME-NAME} cb_repres).
      RUN setFornecedores       IN hBoRepresEmpresa(INPUT FRAME {&FRAME-NAME} fiFornecedores).
      RUN setDtEmissao          IN hBoRepresEmpresa(INPUT FRAME {&FRAME-NAME} fi_dt_ini, INPUT FRAME {&FRAME-NAME} fi_dt_fim).      
      RUN buscarNfsTit          IN hBoRepresEmpresa.
      RUN retornarNfsTit        IN hBoRepresEmpresa(OUTPUT TABLE ttDocs).     
      RUN setTTAux.
  END.
  
  RUN setNfsTit             IN hBoRepres(INPUT TABLE ttAux).
  RUN exportarNfsTit        IN hBoRepres( SESSION:TEMP-DIRECTORY  + "repres_pagto.txt").
  RUN sumarizarMesEmissao   IN hBoRepres.
  RUN exportarMesEmissao    IN hBoRepres(SESSION:TEMP-DIRECTORY  + "repres_pagto_mes_emissao.txt").
  RUN retornarQtMeses       IN hBoRepres(OUTPUT iQtMeses,OUTPUT iQtMesesIMA, OUTPUT iQtMesesMED).
  RUN retornarvlTotal       IN hBoRepres(OUTPUT dValorNota, OUTPUT dValorLiquido,
                                         OUTPUT dValorNotaIMA, OUTPUT dValorLiquidoIMA,
                                         OUTPUT dValorNotaMED, OUTPUT dValorLiquidoMED).
  RUN exportarTotais(SESSION:TEMP-DIRECTORY + "repres_pagto_totais.txt").
  RUN exportarParametros(SESSION:TEMP-DIRECTORY + "repres_pagto_parametros.txt").
  
  RUN esapi\abrirExcel.p('excel\repres_media.xltx').
  
  IF VALID-HANDLE(hBoRepresEmpresa) THEN   DO:
     DELETE PROCEDURE hBoRepresEmpresa.      
  END.  
 
END.

//hBoAppserver:DISCONNECT().

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_repres w-livre
ON VALUE-CHANGED OF cb_repres IN FRAME f-cad /* Representante Externo */
DO:
   FIND FIRST ttRepres
       WHERE ttRepres.codRepres = INPUT FRAME {&FRAME-NAME} cb_repres
       NO-ERROR.
   IF AVAIL ttRepres THEN
      ASSIGN fiFornecedores:SCREEN-VALUE = string(ttRepres.codEmitente).
   
   RUN getFornecsPorMatriz IN hBoRepres( int(fiFornecedores:SCREEN-VALUE),
                                             OUTPUT cOutrosFornecs).
   IF cOutrosFornecs <> '' THEN
      ASSIGN fiFornecedores:SCREEN-VALUE = fiFornecedores:SCREEN-VALUE + "," + cOutrosFornecs.

   

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
             cb_repres:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY cb_repres fiFornecedores fi_dt_ini fi_dt_fim 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button cb_repres fiFornecedores fi_dt_ini fi_dt_fim btExecutar 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportarParametros w-livre 
PROCEDURE exportarParametros :
DEFINE INPUT PARAMETER cArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDtIni AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
DEFINE VARIABLE cDtFim AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
ASSIGN cDtIni = string(INPUT FRAME {&FRAME-NAME} fi_dt_ini:SCREEN-VALUE )
       cDtFim = string(INPUT FRAME {&FRAME-NAME} fi_dt_fim:SCREEN-VALUE ).


OUTPUT TO VALUE(cArquivo).

FIND FIRST ttRepres
      WHERE ttRepres.codRepres = int(cb_repres:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  PUT cb_repres:SCREEN-VALUE IN FRAME {&FRAME-NAME} "-" ttRepres.nomeRepres SKIP.
  PUT "Dt.Emiss∆o Entre " cDtIni " E " cDtFim SKIP.

  OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportarTotais w-livre 
PROCEDURE exportarTotais :
DEFINE INPUT PARAMETER cArquivo AS CHARACTER   NO-UNDO.

OUTPUT TO VALUE(cArquivo).
PUT  "Quantidade Meses|" iQtMeses "|" iQtMesesIMA "|" iQtMesesMED           SKIP  
     "TOTAL Valor Bruto|" dValorNota "|" dValorNotaIMA "|" dValorNotaMED    SKIP 
     "TOTAL Valor Liquido Pago|" dValorLiquido "|" dValorLiquidoIMA "|" dValorLiquidoMED    SKIP. 
OUTPUT CLOSE.


 

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

  IF VALID-HANDLE(hboRepres) THEN  DO:
     DELETE PROCEDURE hBoRepres.     
  END.
  IF VALID-HANDLE(hBoAppServer) THEN  DO:
     RUN desconectarAppServer IN hBoAppServer. 
     DELETE PROCEDURE hBoAppServer.      
  END.
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

  {utp/ut9000.i "ESAP500" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  RUN esbo/boAppServer.p PERSIST SET hboAppServer.
  RUN limparTTEmpresa       IN hBoAppServer.
  RUN incluirEmpresa        IN hBoAppserver('1').
  RUN incluirEmpresa        IN hBoAppserver('5').
  RUN setAppservers         IN hBoAppServer.
  RUN getTTEmpresa          IN hBoAppServer(OUTPUT TABLE ttEmpresa).
  
  RUN esbo/bo_repres.p PERSISTENT SET hBoRepres .
  RUN buscarRepresentantes IN hBoRepres.
  RUN retornarRepresentantes IN hBoRepres(OUTPUT TABLE ttRepres).
  FOR EACH ttRepres.
      RUN esapi/incr_valor_lista.p(cLista,ttRepres.nomeRepres + "(" + string(ttRepres.codRepres) + ")," + string(ttRepres.codRepres),',',OUTPUT cLista).
  END.
  /*MESSAGE cLista
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  ASSIGN cb_repres:LIST-ITEM-PAIRS = cLista.

 

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTTAux w-livre 
PROCEDURE setTTAux :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ttDocs:
    CREATE ttAux.
    BUFFER-COPY ttDocs TO ttAux.
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

