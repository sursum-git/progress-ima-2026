&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

DEFINE VARIABLE num-cheque-ini      AS INTEGER     NO-UNDO.
DEFINE VARIABLE num-cheque-fim      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cod-cta-corren-ini  AS CHAR FORMAT 'x(20)'   NO-UNDO.
DEFINE VARIABLE cod-cta-corren-fim  AS CHAR FORMAT 'x(20)'   NO-UNDO.

DEFINE TEMP-TABLE tt
    FIELD num_cheque        AS INT
    FIELD dat_emis_cheq     AS DATE
    FIELD val_cheque        AS DEC
    FIELD cod_cta_corren    AS CHAR FORMAT 'x(20)'
    FIELD num_talon_cheq    AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-7

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt

/* Definitions for BROWSE BROWSE-7                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-7 tt.cod_cta_corren tt.num_cheque tt.dat_emis_cheq tt.val_cheque   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-7   
&Scoped-define SELF-NAME BROWSE-7
&Scoped-define QUERY-STRING-BROWSE-7 FOR EACH tt
&Scoped-define OPEN-QUERY-BROWSE-7 OPEN QUERY {&SELF-NAME} FOR EACH tt.
&Scoped-define TABLES-IN-QUERY-BROWSE-7 tt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-7 tt


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-7}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button fi-cta-corren fi-num-cheque ~
bt-pesquisar BROWSE-7 bt-executar 
&Scoped-Define DISPLAYED-OBJECTS fi-cta-corren fi-num-cheque 

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
DEFINE BUTTON bt-executar 
     LABEL "Voltar Cheque" 
     SIZE 15 BY 1.13.

DEFINE BUTTON bt-pesquisar 
     LABEL "Pesquisar" 
     SIZE 15 BY 1.

DEFINE VARIABLE fi-cta-corren AS CHARACTER FORMAT "x(10)" 
     LABEL "Conta Corrente" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-num-cheque AS INTEGER FORMAT ">>>>,>>>,>>9" INITIAL 0 
     LABEL "Num Cheque" 
     VIEW-AS FILL-IN 
     SIZE 14.86 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-7 FOR 
      tt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 w-livre _FREEFORM
  QUERY BROWSE-7 DISPLAY
      tt.cod_cta_corren COLUMN-LABEL "Cta.Corren"
 tt.num_cheque     COLUMN-LABEL "Numero"
 tt.dat_emis_cheq  COLUMN-LABEL "Dt.Emiss∆o"
 tt.val_cheque     COLUMN-LABEL "Valor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 76 BY 4.5
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi-cta-corren AT ROW 3.92 COL 15.57 COLON-ALIGNED HELP
          "C¢digo Conta Corrente" WIDGET-ID 4
     fi-num-cheque AT ROW 4 COL 45 COLON-ALIGNED HELP
          "N£mero Cheque" WIDGET-ID 6
     bt-pesquisar AT ROW 4 COL 66 WIDGET-ID 8
     BROWSE-7 AT ROW 5.75 COL 5 WIDGET-ID 200
     bt-executar AT ROW 10.5 COL 5 WIDGET-ID 10
     "ATENÄ«O! Utilize apenas para cheques que n∆o movimentaram o banco." VIEW-AS TEXT
          SIZE 79 BY .75 AT ROW 2.75 COL 4 WIDGET-ID 12
          BGCOLOR 15 FGCOLOR 12 FONT 21
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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 10.79
         WIDTH              = 89.43
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
/* BROWSE-TAB BROWSE-7 bt-pesquisar f-cad */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-7
/* Query rebuild information for BROWSE BROWSE-7
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-7 */
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


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-livre
ON CHOOSE OF bt-executar IN FRAME f-cad /* Voltar Cheque */
DO:
  IF AVAIL tt THEN DO:
      DO TRANSACTION:
         FIND FIRST ems5.cheque WHERE
                    ems5.cheque.num_cheque = tt.num_cheque EXCLUSIVE-LOCK NO-ERROR.
         
         IF AVAIL ems5.cheque THEN DO:
            FIND cheq_ap OF cheque EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL cheq_ap THEN DO:
              FOR EACH ITEM_cheq_ap OF cheq_ap:
                  DELETE ITEM_cheq_ap.
              END.
              DELETE cheq_ap.
            END.
                
            DELETE cheque.
            FIND FIRST talon_cheq WHERE
              talon_cheq.num_talon_cheq = tt.num_talon_cheq AND
              talon_cheq.cod_cta_corren = tt.cod_cta_corren EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL talon_cheq THEN DO:
               IF talon_cheq.num_ult_cheq_emitid >= tt.num_cheque THEN
                  ASSIGN  talon_cheq.num_ult_cheq_emitid = tt.num_cheque - 1
                          talon_cheq.num_prox_cheq  = tt.num_cheque .

            END.
            RELEASE talon_cheq.
            MESSAGE 'O cheque:' tt.num_cheque ' foi removido do sistema. Favor gera-lo novamente'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY 'choose' TO bt-pesquisar.
            END.
         END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pesquisar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pesquisar w-livre
ON CHOOSE OF bt-pesquisar IN FRAME f-cad /* Pesquisar */
DO:

/*   IF fi-cta-corren:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''  THEN DO: */
/*       MESSAGE 'Favor preencher conta corrente....'                    */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                          */
/*        APPLY 'ENTRY' TO fi-cta-corren IN FRAME {&FRAME-NAME}.         */
/*       RETURN NO-APPLY.                                                */
/*   END.                                                                */
  IF fi-num-cheque:SCREEN-VALUE = "0"  THEN

      ASSIGN num-cheque-ini = 0 
             num-cheque-fim = 99999999.
  ELSE
     ASSIGN  num-cheque-ini = INPUT FRAME {&frame-name} fi-num-cheque
             num-cheque-fim = INPUT FRAME {&frame-name} fi-num-cheque.

 IF fi-cta-corren:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""  THEN

      ASSIGN cod-cta-corren-ini = '' 
             cod-cta-corren-fim = 'zzzzzzzzzzzzzz'.
  ELSE
     ASSIGN  cod-cta-corren-ini = fi-cta-corren:SCREEN-VALUE IN FRAME {&FRAME-NAME}
             cod-cta-corren-fim = fi-cta-corren:SCREEN-VALUE IN FRAME {&FRAME-NAME}.


  EMPTY TEMP-TABLE tt.
  FOR EACH ems5.cheque NO-LOCK
      WHERE cheque.num_cheque >= num-cheque-ini
      AND   cheque.num_cheque <= num-cheque-fim
      AND   cheque.cod_cta_corren >= cod-cta-corren-ini
      AND   cheque.cod_cta_corren <= cod-cta-corren-fim.
      CREATE tt.
      ASSIGN tt.num_cheque     = cheque.num_cheque
             tt.cod_cta_corren = cheque.cod_cta_corren
             tt.dat_emis_cheq  = cheque.dat_emis_cheq
             tt.val_cheque     = cheque.val_cheque.
  END.
  {&OPEN-QUERY-BROWSE-7}
  
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


&Scoped-define BROWSE-NAME BROWSE-7
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
             fi-cta-corren:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY fi-cta-corren fi-num-cheque 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fi-cta-corren fi-num-cheque bt-pesquisar BROWSE-7 
         bt-executar 
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
  {src/adm/template/snd-list.i "tt"}

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

