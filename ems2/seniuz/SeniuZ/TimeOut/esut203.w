&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-cadsim 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*
{include/i-prgvrs.i XX9999 9.99.99.999}
*/
/*------------------------------------------------------------------------

  File:

  Description:

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Version: 1.00.000

  History:

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR i-erros     AS INTEGER.
DEF VAR l-mestre    AS LOGICAL.

DEF VAR password        AS CHAR     NO-UNDO.

DEF TEMP-TABLE tt-param    NO-UNDO
    FIELD destino          AS INT
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INT
    FIELD cod-usuario      AS CHAR
    FIELD cod-usua-solic   AS CHAR
    FIELD dat-solic        AS DATE
    FIELD hor-solic        AS INT.

DEF TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem            AS INT  FORMAT ">>>>9"
    FIELD exemplo          AS CHAR FORMAT "x(30)"
    INDEX id ordem.

DEF BUFFER b-tt-digita FOR tt-digita.

DEF VAR raw-param        as raw no-undo.

DEF TEMP-TABLE tt-raw-digita
   FIELD raw-digita      AS RAW.

{btb/btb912zb.i}
{utp/ut-glob.i}

{util\inf_sistema.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-paruni
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-13 rt-button c-usuario c-senha ~
bt-cancela w-nome 
&Scoped-Define DISPLAYED-OBJECTS c-usuario c-senha c-usuario-desc w-nome 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-cadsim AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Fechar Programa" 
     SIZE 18 BY 1 TOOLTIP "Clique para cancelar o pedido de desconecá∆o de usu†rio."
     BGCOLOR 8 .

DEFINE BUTTON bt-ok 
     LABEL "&Executar" 
     SIZE 11 BY 1 TOOLTIP "Clique para processar o pedido de desconecá∆o de usu†rio."
     BGCOLOR 8 .

DEFINE VARIABLE c-usuario-desc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Computador a ser Desconectado" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 20 BY 1 TOOLTIP "Informe o nome do computador que dever† ser desconectado"
     FONT 2 NO-UNDO.

DEFINE VARIABLE c-senha AS CHARACTER FORMAT "X(12)":U 
     LABEL "Senha" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Informe a senha do usu†rio no EMS 2.02"
     FONT 2 NO-UNDO.

DEFINE VARIABLE c-usuario AS CHARACTER FORMAT "X(12)":U 
     LABEL "Seu usu†rio" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Informe o nome do usu†rio do EMS 2.02"
     FONT 2 NO-UNDO.

DEFINE VARIABLE w-nome AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 31.57 BY .67
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 4.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 52.86 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     c-usuario AT ROW 1.5 COL 19 COLON-ALIGNED HELP
          "Informe o nome do usu†rio do EMS 2.02"
     c-senha AT ROW 3.75 COL 19 COLON-ALIGNED HELP
          "Informe a senha do usu†rio no EMS 2.02"
     c-usuario-desc AT ROW 5.5 COL 31 COLON-ALIGNED HELP
          "Informe o nome do computador que dever† ser desconectado"
     bt-ok AT ROW 7.25 COL 2 HELP
          "Clique para processar o pedido de desconecá∆o de usu†rio."
     bt-cancela AT ROW 7.25 COL 35 HELP
          "Clique para cancelar o pedido de desconecá∆o de usu†rio."
     w-nome AT ROW 2.75 COL 21 NO-LABEL
     RECT-12 AT ROW 1 COL 1
     RECT-13 AT ROW 5.25 COL 1
     rt-button AT ROW 7 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.29 BY 7.5
         FONT 2.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-paruni
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-cadsim ASSIGN
         HIDDEN             = YES
         TITLE              = "Solicitaá∆o de desconex∆o do EMS 2.02/Sistema Toshiba"
         HEIGHT             = 7.5
         WIDTH              = 53.43
         MAX-HEIGHT         = 7.5
         MAX-WIDTH          = 57.57
         VIRTUAL-HEIGHT     = 7.5
         VIRTUAL-WIDTH      = 57.57
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 2
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-cadsim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-incsim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-cadsim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-ok IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX c-usuario-desc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-nome IN FRAME f-cad
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-cadsim)
THEN w-cadsim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-cadsim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON END-ERROR OF w-cadsim /* Solicitaá∆o de desconex∆o do EMS 2.02/Sistema Toshiba */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cadsim w-cadsim
ON WINDOW-CLOSE OF w-cadsim /* Solicitaá∆o de desconex∆o do EMS 2.02/Sistema Toshiba */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  RUN notify ('cancel-record').
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-cadsim
ON CHOOSE OF bt-cancela IN FRAME f-cad /* Fechar Programa */
DO:
  RUN notify ('cancel-record').
  
/*  APPLY "close" to this-procedure.*/
  
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      QUIT.
  &ENDIF

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-cadsim
ON CHOOSE OF bt-ok IN FRAME f-cad /* Executar */
DO:
    
    IF INPUT c-usuario-desc = "super" THEN DO:
        RUN esp/util/mensagem (INPUT "Usu†rio Inv†lido!",
                               INPUT "O usu†rio SUPER n∆o pode ser desconectado.").
        RETURN NO-APPLY.
    END.

    FIND mgcad.usuar_mestre
        WHERE mgcad.usuar_mestre.cod_usuario = c-usuario:SCREEN-VALUE NO-LOCK NO-ERROR.
    
    IF NOT AVAIL mgcad.usuar_mestre THEN DO:
        RUN esp/util/mensagem (INPUT "Usu†rio n∆o Cadastrado!",
                               INPUT "O c¢digo do usu†rio n∆o est† cadastrado no Sistema.").
        ASSIGN i-erros = i-erros + 1.
        IF i-erros > 5 THEN 
            APPLY "choose" TO bt-cancela.
        RETURN NO-APPLY.
    END.
    
    IF ENCODE(LC(password)) <> mgcad.usuar_mestre.cod_senha THEN DO:
        RUN esp/util/mensagem (INPUT "Senha Inv†lida!",
                               INPUT "A Senha informada n∆o confere com a senha do usu†rio.").
        ASSIGN i-erros = i-erros + 1.
        IF i-erros > 5 THEN 
            APPLY "choose" TO bt-cancela.
        RETURN NO-APPLY.
    END.
    
    IF INPUT c-usuario-desc = "" THEN DO:
        RUN esp/util/mensagem (INPUT "Computador Inv†lido!",
                               INPUT "O nome do computador deve ser informado.").
        RETURN NO-APPLY.
    END.

    RUN pi-executa-rpw.
           
    RUN esp/util/mensagem (INPUT "Solicitaá∆o Registrada!",
                           INPUT "Solicitamos aguardar de 1 a 2 minutos, tempo necess†rio para que o programa possa derrubar o usu†rio.").

    IF CAN-FIND (FIRST usuar_grp_usuar
                 WHERE usuar_grp_usuar.cod_grp_usuar = "KIL"
                   AND usuar_grp_usuar.cod_usuario   = INPUT c-usuario) THEN DO:
      ASSIGN c-usuario-desc:SENSITIVE = YES.
      APPLY "entry" TO c-usuario-desc.
      RETURN NO-APPLY.
    END.

    &IF DEFINED (adm-panel) <> 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
        QUIT.
    &ENDIF
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-senha
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-senha w-cadsim
ON ANY-KEY OF c-senha IN FRAME f-cad /* Senha */
DO:

    CASE KEYFUNCTION(LASTKEY):

        /*** Remove last char from password and last "*" from fill-in. ***/
        WHEN "DELETE-CHARACTER" THEN DO:
            IF LENGTH(password) > 0 THEN DO:
                ASSIGN password              = SUBSTRING(password,1,(LENGTH(password) - 1))
                       c-senha:SCREEN-VALUE  = SUBSTRING(c-senha:SCREEN-VALUE,1,
                                                         LENGTH(c-senha:SCREEN-VALUE) - 1)
                       c-senha:CURSOR-OFFSET = LENGTH(c-senha:SCREEN-VALUE) + 1.
                RETURN NO-APPLY.
            END.
        END.

        /*** Remove last char from password and last "*" from fill-in. ***/
        WHEN "BACKSPACE" THEN DO:
            IF LENGTH(password) > 0 THEN DO:
                ASSIGN password = SUBSTRING(password,1,(LENGTH(password) - 1))
                       c-senha:SCREEN-VALUE  = SUBSTRING(c-senha:SCREEN-VALUE,1,
                                                         LENGTH(c-senha:SCREEN-VALUE) - 1).
                       c-senha:CURSOR-OFFSET = LENGTH(c-senha:SCREEN-VALUE) + 1.
                RETURN NO-APPLY.
            END.
        END.

        /*** User believes password is correct -- validate password. ***/
        WHEN "RETURN" THEN DO:
            /*** Your validation code here. ***/
            APPLY "LEAVE":U TO SELF.
            RETURN NO-APPLY.
        END.

        /*** User believes password is correct -- validate password. ***/
        WHEN "TAB" THEN DO:
            /*** Your validation code here. ***/
            APPLY "LEAVE":U TO SELF.
            RETURN NO-APPLY.
        END.

        /*** END-ERROR ***/
        WHEN "END-ERROR" THEN DO:

            APPLY "CHOOSE" TO bt-cancela IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.

        END.

        WHEN "" THEN DO:
            ASSIGN password = ""
                   c-senha:SCREEN-VALUE = "".
            RETURN NO-APPLY.
        END.

        /*** check for printable character ***/
        OTHERWISE DO:

            IF KEYCODE(KEYFUNCTION(LASTKEY)) > 0   AND
               KEYCODE(KEYFUNCTION(LASTKEY)) < 200 THEN DO:

                /*** Build password from input & display "*" in fill-in. ***/

                ASSIGN password              = password + KEYFUNCTION(LASTKEY)
                       c-senha:SCREEN-VALUE  = c-senha:SCREEN-VALUE + "*"
                       c-senha:CURSOR-OFFSET = LENGTH(c-senha:SCREEN-VALUE) + 1.
                RETURN NO-APPLY.
            END.
            ELSE 
                RETURN NO-APPLY. /* ignore non-printables */

        END.
        
    END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-senha w-cadsim
ON LEAVE OF c-senha IN FRAME f-cad /* Senha */
DO:
    
    FIND mgcad.usuar_mestre
        WHERE mgcad.usuar_mestre.cod_usuario = c-usuario:SCREEN-VALUE NO-LOCK NO-ERROR.
    
    IF NOT AVAIL mgcad.usuar_mestre THEN DO:
        RUN esp/util/mensagem (INPUT "Usu†rio n∆o Cadastrado!",
                               INPUT "O c¢digo do usu†rio n∆o est† cadastrado no Sistema.").
        ASSIGN i-erros = i-erros + 1.
        IF i-erros > 5
            THEN APPLY "choose" TO bt-cancela.
        RETURN NO-APPLY.
    END.
    
    IF ENCODE(LC(password)) <> mgcad.usuar_mestre.cod_senha THEN DO:
        RUN esp/util/mensagem (INPUT "Senha Inv†lida!",
                               INPUT "A senha informada n∆o confere com a senha cadastrada.").
        ASSIGN i-erros = i-erros + 1.
        IF i-erros > 5
            THEN APPLY "choose" TO bt-cancela.
        APPLY "entry" to c-usuario.
        RETURN NO-APPLY.
    END.
    
    ASSIGN c-usuario-desc:SCREEN-VALUE = CAPS(computador())
        bt-ok:SENSITIVE                = YES
        c-senha:SENSITIVE              = NO
        c-usuario:SENSITIVE            = NO.
    
    IF CAN-FIND (FIRST usuar_grp_usuar
                 WHERE usuar_grp_usuar.cod_grp_usuar = "KIL"
                   AND usuar_grp_usuar.cod_usuario   = INPUT c-usuario) THEN
        ASSIGN c-usuario-desc:SENSITIVE = YES.
    
    APPLY "entry" TO c-usuario-desc.
    
    RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c-usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c-usuario w-cadsim
ON LEAVE OF c-usuario IN FRAME f-cad /* Seu usu†rio */
DO:
   
    FIND mgcad.usuar_mestre
        WHERE mgcad.usuar_mestre.cod_usuario = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    
    IF NOT AVAIL mgcad.usuar_mestre THEN DO:
        RUN esp/util/mensagem (INPUT "Usu†rio n∆o Cadastrado!",
                               INPUT "O c¢digo do usu†rio n∆o est† cadastrado no Sistema.").
        ASSIGN i-erros = i-erros + 1.
        IF i-erros > 5 THEN 
            APPLY "choose" TO bt-cancela.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN w-nome:SCREEN-VALUE      = mgcad.usuar_mestre.nom_usuario
            c-usuario-desc:SCREEN-VALUE = CAPS(computador()).
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-cadsim 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

SESSION:DATA-ENTRY-RETURN = YES.

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
  DISPLAY c-usuario c-senha c-usuario-desc w-nome 
      WITH FRAME f-cad IN WINDOW w-cadsim.
  ENABLE RECT-12 RECT-13 rt-button c-usuario c-senha bt-cancela w-nome 
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

  quit.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  FIND FIRST servid_exec
       WHERE servid_exec.cod_servid_exec = "Sistemas" EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

  IF NOT LOCKED servid_exec THEN DO:
      RUN esp/util/mensagem (INPUT "Servidor RPWSis n∆o ativado!",
                             INPUT "Para utilizar o programa de desconecta usu†rio do EMS 2.02 e Sistema Toshiba o RPWSis dever† estar ativado. No momento encontra-se desativado, favor avisar ao CPD - 6351").
      QUIT.
  END.


  DEF VAR w-comput      AS CHAR NO-UNDO.

  ASSIGN w-comput = "".

  FOR EACH tosmdo._connect:
      IF tosmdo._connect._connect-device <> ?       AND
         tosmdo._connect._connect-device <> "batch" AND
         tosmdo._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, tosmdo._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(tosmdo._connect._connect-device) + "," + CAPS(tosmdo._connect._connect-device) ELSE w-comput + "," + CAPS(tosmdo._connect._connect-device) + "," + CAPS(tosmdo._connect._connect-device).
  END.

  FOR EACH vendas._connect:
      IF vendas._connect._connect-device <> ?       AND
         vendas._connect._connect-device <> "batch" AND
         vendas._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, vendas._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(vendas._connect._connect-device) + "," + CAPS(vendas._connect._connect-device) ELSE w-comput + "," + CAPS(vendas._connect._connect-device) + "," + CAPS(vendas._connect._connect-device).
  END.

  FOR EACH mgcad._connect:
      IF mgcad._connect._connect-device <> ?       AND
         mgcad._connect._connect-device <> "batch" AND
         mgcad._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, mgcad._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(mgcad._connect._connect-device) + "," + CAPS(mgcad._connect._connect-device) ELSE w-comput + "," + CAPS(mgcad._connect._connect-device) + "," + CAPS(mgcad._connect._connect-device).
  END.
  
  FOR EACH mgcad._connect:
      IF mgcad._connect._connect-device <> ?       AND
         mgcad._connect._connect-device <> "batch" AND
         mgcad._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, mgcad._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(mgcad._connect._connect-device) + "," + CAPS(mgcad._connect._connect-device) ELSE w-comput + "," + CAPS(mgcad._connect._connect-device) + "," + CAPS(mgcad._connect._connect-device).
  END.

  IF NOT CONNECTED("finmov") THEN DO:
      IF PDBNAME("mguni") = "tems2cad" THEN                                                              
         RUN esp/funcoes/conecta-bco-ext.p (INPUT "tems5mov",  /* Nome Fisico do Banco */                
                                            INPUT "finmov",  /* Nome Logico do Banco */                  
                                            INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */      
      ELSE                                                                                               
         RUN esp/funcoes/conecta-bco-ext.p (INPUT "ems5mov",  /* Nome Fisico do Banco */                 
                                            INPUT "finmov",  /* Nome Logico do Banco */                  
                                            INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */      

  END.
  IF NOT CONNECTED("fincad") THEN DO:
  IF PDBNAME("mguni") = "tems2cad" THEN
     RUN esp/funcoes/conecta-bco-ext.p (INPUT "tems5cad",  /* Nome Fisico do Banco */
                                        INPUT "fincad",  /* Nome Logico do Banco */
                                        INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */
  ELSE
     RUN esp/funcoes/conecta-bco-ext.p (INPUT "ems5cad",  /* Nome Fisico do Banco */
                                        INPUT "fincad",  /* Nome Logico do Banco */
                                        INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */
  END.


  CONNECT VALUE("-db hr209    -ld hr209    -S 30000 -H 172.19.0.20") NO-ERROR.
  CONNECT VALUE("-db dthrtosh -ld dthrtosh -S 30100 -H 172.19.0.20") NO-ERROR.
  CONNECT VALUE("-db ems2uni  -ld ems2uni  -S 30200 -H 172.19.0.20") NO-ERROR.

  RUN esp/utp/esut203e.r (INPUT-OUTPUT w-comput).
/*
  FOR EACH FINCAD._connect:
      IF FINCAD._connect._connect-device <> ?       AND
         FINCAD._connect._connect-device <> "batch" AND
         FINCAD._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, FINCAD._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(FINCAD._connect._connect-device) + "," + CAPS(FINCAD._connect._connect-device) ELSE w-comput + "," + CAPS(FINCAD._connect._connect-device) + "," + CAPS(FINCAD._connect._connect-device).
  END.

  FOR EACH FINMOV._connect:
      IF FINMOV._connect._connect-device <> ?       AND
         FINMOV._connect._connect-device <> "batch" AND
         FINMOV._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, FINMOV._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(FINMOV._connect._connect-device) + "," + CAPS(FINMOV._connect._connect-device) ELSE w-comput + "," + CAPS(FINMOV._connect._connect-device) + "," + CAPS(FINMOV._connect._connect-device).
  END.
  FOR EACH HR209._connect:
      IF HR209._connect._connect-device <> ?       AND
         HR209._connect._connect-device <> "batch" AND
         HR209._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, HR209._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(HR209._connect._connect-device) + "," + CAPS(HR209._connect._connect-device) ELSE w-comput + "," + CAPS(HR209._connect._connect-device) + "," + CAPS(HR209._connect._connect-device).
  END.
  FOR EACH dthrtosh._connect:
      IF dthrtosh._connect._connect-device <> ?       AND
         dthrtosh._connect._connect-device <> "batch" AND
         dthrtosh._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, dthrtosh._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(dthrtosh._connect._connect-device) + "," + CAPS(dthrtosh._connect._connect-device) ELSE w-comput + "," + CAPS(dthrtosh._connect._connect-device) + "," + CAPS(dthrtosh._connect._connect-device).
  END.
  FOR EACH ems2uni._connect:
      IF ems2uni._connect._connect-device <> ?       AND
         ems2uni._connect._connect-device <> "batch" AND
         ems2uni._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, ems2uni._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(ems2uni._connect._connect-device) + "," + CAPS(ems2uni._connect._connect-device) ELSE w-comput + "," + CAPS(ems2uni._connect._connect-device) + "," + CAPS(ems2uni._connect._connect-device).
  END.*/

    DISCONNECT fincad   NO-ERROR.
    DISCONNECT finmov   NO-ERROR.
    DISCONNECT hr209    NO-ERROR.
    DISCONNECT dthrtosh NO-ERROR.
    DISCONNECT ems2uni  NO-ERROR.


  ASSIGN c-usuario-desc:LIST-ITEM-PAIRS IN FRAME f-cad = w-comput NO-ERROR.

/*
  IF NOT CONNECTED("finmov") THEN DO:
      IF PDBNAME("mguni") = "tems2cad" THEN                                                              
         RUN esp/funcoes/conecta-bco-ext.p (INPUT "tems5mov",  /* Nome Fisico do Banco */                
                                            INPUT "finmov",  /* Nome Logico do Banco */                  
                                            INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */      
      ELSE                                                                                               
         RUN esp/funcoes/conecta-bco-ext.p (INPUT "ems5mov",  /* Nome Fisico do Banco */                 
                                            INPUT "finmov",  /* Nome Logico do Banco */                  
                                            INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */      

  END.
  IF NOT CONNECTED("fincad") THEN DO:
  IF PDBNAME("mguni") = "tems2cad" THEN
     RUN esp/funcoes/conecta-bco-ext.p (INPUT "tems5cad",  /* Nome Fisico do Banco */
                                        INPUT "fincad",  /* Nome Logico do Banco */
                                        INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */
  ELSE
     RUN esp/funcoes/conecta-bco-ext.p (INPUT "ems5cad",  /* Nome Fisico do Banco */
                                        INPUT "fincad",  /* Nome Logico do Banco */
                                        INPUT ""). /* Outros Parametros - Usuario, Senha, Mm */
  END.


  CONNECT VALUE("-db hr209    -ld hr209    -S 30000 -H 172.19.0.20") NO-ERROR.
  CONNECT VALUE("-db dthrtosh -ld dthrtosh -S 30100 -H 172.19.0.20") NO-ERROR.
  CONNECT VALUE("-db ems2uni  -ld ems2uni  -S 30200 -H 172.19.0.20") NO-ERROR.

    DISCONNECT fincad   NO-ERROR.
    DISCONNECT finmov   NO-ERROR.
    DISCONNECT hr209    NO-ERROR.
    DISCONNECT dthrtosh NO-ERROR.
    DISCONNECT ems2uni  NO-ERROR.
*/


  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}
      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executa-rpw w-cadsim 
PROCEDURE pi-executa-rpw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-param.
        DELETE tt-param.
    END.

    FOR EACH tt_param_segur:
        DELETE tt_param_segur.
    END.

    FOR EACH tt_ped_exec:
        DELETE tt_ped_exec.
    END.

    FOR EACH tt_ped_exec_param:
        DELETE tt_ped_exec_param.
    END.

    FOR EACH tt_ped_exec_param_aux:
        DELETE tt_ped_exec_param_aux.
    END.

    FOR EACH tt_ped_exec_sel:
        DELETE tt_ped_exec_sel.
    END.

    CREATE tt-param.
    ASSIGN tt-param.cod-usuario     = INPUT FRAME {&FRAME-NAME} c-usuario-desc
           tt-param.dat-solic       = TODAY
           tt-param.hor-solic       = TIME
           tt-param.cod-usua-solic  = INPUT FRAME {&FRAME-NAME} c-usuario
           tt-param.usuario         = c-usuario
           tt-param.destino         = 2
           tt-param.arquivo         = "der_" + STRING(YEAR(TODAY),"9999") + "_" + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY),"99") +   ".lst"
           tt-param.data-exec       = TODAY
           tt-param.hora-exec       = TIME
           v_cod_usuar_corren       = INPUT c-usuario.
    
    RAW-TRANSFER tt-param TO raw-param.
    FOR EACH tt-digita NO-LOCK:
        CREATE tt-raw-digita.
        RAW-TRANSFER tt-digita TO tt-raw-digita.raw-digita.
    END.

    CREATE tt_param_segur.
    ASSIGN tta_num_vers_integr_api                      = 3
        tt_param_segur.tta_cod_aplicat_dtsul_corren     = "TEC"
        tt_param_segur.tta_cod_ccusto_corren            = ""
        tt_param_segur.tta_cod_dwb_user                 = ""
        tt_param_segur.tta_cod_empres_usuar             = "02"
        tt_param_segur.tta_cod_estab_usuar              = ""
        tt_param_segur.tta_cod_funcao_negoc_empres      = ""
        tt_param_segur.tta_cod_grp_usuar_lst            = "SUP"
        tt_param_segur.tta_cod_idiom_usuar              = "POR"
        tt_param_segur.tta_cod_modul_dtsul_corren       = "MNU"
        tt_param_segur.tta_cod_modul_dtsul_empres       = "MNU"
        tt_param_segur.tta_cod_pais_empres_usuar        = "Brasil"
        tt_param_segur.tta_cod_plano_ccusto_corren      = ""
        tt_param_segur.tta_cod_unid_negoc_usuar         = ""
        tt_param_segur.tta_cod_usuar_corren             = v_cod_usuar_corren
        tt_param_segur.tta_cod_usuar_corren_criptog     = ENCODE(v_cod_usuar_corren)
        tt_param_segur.tta_num_ped_exec_corren          = 0
        tt_param_segur.tta_rec_ped_exec                 = ?
        tt_param_segur.ttv_num_msg_erro                 = 0
        tt_param_segur.ttv_des_msg                      = "".
           
    CREATE tt_ped_exec.
    ASSIGN 
        tt_ped_exec.tta_num_seq                         = 1
        tt_ped_exec.tta_num_ped_exec                    = 0
        tt_ped_exec.tta_cod_usuario                     = v_cod_usuar_corren
        tt_ped_exec.tta_cod_prog_dtsul                  = "esut203"
        tt_ped_exec.tta_cod_prog_dtsul_rp               = "esp/utp/esut203r.p"
        tt_ped_exec.tta_cod_release_prog_dtsul          = "2.02.00.001"
        tt_ped_exec.tta_dat_exec_ped_exec               = TODAY
        tt_ped_exec.tta_hra_exec_ped_exec               = SUBSTRING(STRING(TIME,"hh:mm:ss"),1,2) + SUBSTRING(STRING(TIME,"hh:mm:ss"),4,2) + SUBSTRING(STRING(TIME,"hh:mm:ss"),7,2)
        tt_ped_exec.tta_num_ped_exec_pai                = 0
        tt_ped_exec.tta_log_exec_prog_depend            = NO
        tt_ped_exec.tta_cod_servid_exec                 = "Sistemas"
        tt_ped_exec.tta_cdn_estil_dwb                   = 97
        tt_ped_exec.ttv_num_msg_erro                    = 0
        tt_ped_exec.ttv_cod_msg_parameters              = "0"
        tt_ped_exec.tta_cod_orig_ped_exec               = "".

    CREATE tt_ped_exec_param.
    ASSIGN tt_ped_exec_param.tta_num_seq                = 1
        tt_ped_exec_param.tta_cod_dwb_parameters        = ""
        tt_ped_exec_param.tta_cod_dwb_file              = "esut203.lst"
        tt_ped_exec_param.tta_cod_dwb_output            = "Arquivo"
        tt_ped_exec_param.tta_cod_dwb_order             = ""
        tt_ped_exec_param.tta_nom_dwb_printer           = "esut203.lst"
        tt_ped_exec_param.tta_cod_dwb_print_layout      = ""
        tt_ped_exec_param.tta_log_dwb_print_parameters  = NO
        tt_ped_exec_param.tta_raw_param_ped_exec        = raw-param.

    RUN btb/btb912zb.p (INPUT-OUTPUT TABLE tt_param_segur,
                        INPUT-OUTPUT TABLE tt_ped_exec,
                        INPUT        TABLE tt_ped_exec_param,
                        INPUT        TABLE tt_ped_exec_param_aux,
                        INPUT        TABLE tt_ped_exec_sel).

    FIND FIRST tt_param_segur NO-LOCK NO-ERROR.
    FIND FIRST tt_ped_exec    NO-LOCK NO-ERROR.

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
     Tables specified for this w-paruni, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-cadsim 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

