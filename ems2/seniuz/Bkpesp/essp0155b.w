&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF BUFFER empresa FOR mgcad.empresa.

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER p-nr-pedcli      LIKE ped-venda.nr-pedcli.

/* Local Variable Definitions ---                                       */

DEF VAR c-hora AS CHAR FORMAT "x(15)".
DEF VAR l-ok   AS LOG.
DEF VAR i-lin  AS INT.
DEF VAR c-empresa AS CHAR.
DEF VAR c-ocorrencia AS CHAR FORMAT "x(500)".

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-historico

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES his-ped-venda-ext

/* Definitions for BROWSE br-historico                                  */
&Scoped-define FIELDS-IN-QUERY-br-historico his-ped-venda-ext.dt-trans ~
string(his-ped-venda-ext.hr-trans,"HH:MM:SS") @ c-hora ~
his-ped-venda-ext.usuario his-ped-venda-ext.ocorrencia 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-historico 
&Scoped-define QUERY-STRING-br-historico FOR EACH his-ped-venda-ext ~
      WHERE his-ped-venda-ext.cod-estabel = ped-venda.cod-estabel ~
 AND his-ped-venda-ext.nr-pedcli = p-nr-pedcli  NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-historico OPEN QUERY br-historico FOR EACH his-ped-venda-ext ~
      WHERE his-ped-venda-ext.cod-estabel = ped-venda.cod-estabel ~
 AND his-ped-venda-ext.nr-pedcli = p-nr-pedcli  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-historico his-ped-venda-ext
&Scoped-define FIRST-TABLE-IN-QUERY-br-historico his-ped-venda-ext


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-historico}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-buttom br-historico bt-ok ~
bt-cancela bt-imprime bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-pedcli fi-cliente fi-nome-cli 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-ocorrencia D-Dialog 
FUNCTION fn-ocorrencia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-cli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 2.42.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 92 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-historico FOR 
      his-ped-venda-ext SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-historico D-Dialog _STRUCTURED
  QUERY br-historico NO-LOCK DISPLAY
      his-ped-venda-ext.dt-trans FORMAT "99/99/9999":U
      string(his-ped-venda-ext.hr-trans,"HH:MM:SS") @ c-hora COLUMN-LABEL "Hora" FORMAT "x(9)":U
            WIDTH 6.57
      his-ped-venda-ext.usuario FORMAT "X(12)":U WIDTH 10
      his-ped-venda-ext.ocorrencia FORMAT "X(200)":U WIDTH 200
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 92 BY 11.25
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-pedcli AT ROW 1.38 COL 10 COLON-ALIGNED
     fi-cliente AT ROW 2.38 COL 10 COLON-ALIGNED
     fi-nome-cli AT ROW 2.38 COL 22.57 COLON-ALIGNED NO-LABEL
     br-historico AT ROW 3.67 COL 2
     bt-ok AT ROW 15.38 COL 3
     bt-cancela AT ROW 15.38 COL 14
     bt-imprime AT ROW 15.38 COL 26
     bt-ajuda AT ROW 15.38 COL 83.14
     RECT-1 AT ROW 1.08 COL 2
     rt-buttom AT ROW 15.13 COL 2
     SPACE(0.99) SKIP(0.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Hist¢rico Alteraá‰es do Pedido - ESSP0155B"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
/* BROWSE-TAB br-historico fi-nome-cli D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cliente IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-cli IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-pedcli IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-historico
/* Query rebuild information for BROWSE br-historico
     _TblList          = "espec.his-ped-venda-ext"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.his-ped-venda-ext.cod-estabel = ped-venda.cod-estabel
 AND espec.his-ped-venda-ext.nr-pedcli = p-nr-pedcli "
     _FldNameList[1]   = espec.his-ped-venda-ext.dt-trans
     _FldNameList[2]   > "_<CALC>"
"string(his-ped-venda-ext.hr-trans,""HH:MM:SS"") @ c-hora" "Hora" "x(9)" ? ? ? ? ? ? ? no ? no no "6.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > espec.his-ped-venda-ext.usuario
"usuario" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > espec.his-ped-venda-ext.ocorrencia
"ocorrencia" ? ? "character" ? ? ? ? ? ? no ? no no "200" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-historico */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Hist¢rico Alteraá‰es do Pedido - ESSP0155B */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
   RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-historico
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-pedcli fi-cliente fi-nome-cli 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 rt-buttom br-historico bt-ok bt-cancela bt-imprime bt-ajuda 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /*{utp/ut9000.i "D99XX999" "9.99.99.999"}*/

  ASSIGN fi-nr-pedcli = p-nr-pedcli.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  FIND ped-venda WHERE
       ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.
       
  FIND emitente WHERE
       emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .


  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.nome-abrev
         fi-nome-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec D-Dialog 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT   1
     "DATA: "                                  AT  77
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  83
     "HORA: "                                  AT 107
     STRING(TIME,"hh:mm:ss")                   AT 113
     SKIP(1).
    
 PUT "RELATORIO DO HIST‡RICO DAS ALTERAÄÂES DO PEDIDO" AT 45 SKIP(1).


 PUT "PEDIDO: "          AT  1
     ped-venda.nr-pedcli AT  9
     "CLIENTE: "         AT 23
     emitente.nome-emit  AT 32 SKIP(1).         
 
 PUT "DATA        HORA     USUARIO      OCORR“NCIA" AT 1.
 PUT "----------- -------- ------------ ----------------------------------------------------------------------------------------------------" AT 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime D-Dialog 
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
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 64.
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0155b.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-lin = 99.
    FOR EACH his-ped-venda-ext WHERE
             his-ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
             his-ped-venda-ext.nr-pedcli = p-nr-pedcli NO-LOCK.
        IF i-lin > 64 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 9.
        END.
        PUT his-ped-venda-ext.dt-trans                     AT  1
            STRING(his-ped-venda-ext.hr-trans, "HH:MM:SS") AT 13
            his-ped-venda-ext.usuario                      AT 22
            SUBSTR(his-ped-venda-ext.ocorrencia, 1, 100)  FORMAT "x(100)"  AT 36 SKIP
            SUBSTR(his-ped-venda-ext.ocorrencia, 101,100) FORMAT "x(100)"  AT 36.
        ASSIGN i-lin = i-lin + 2.
   END.
   IF i-saida = 1 THEN DO:
      PAGE.
      PUT "" AT 1.
   END.
 END.
 IF i-saida = 3 THEN DO.
   RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                         INPUT c-saida).
   DELETE PROCEDURE h-prog.
 END.
 OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "his-ped-venda-ext"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-ocorrencia D-Dialog 
FUNCTION fn-ocorrencia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN his-ped-venda-ext.ocorrencia + ped-venda.desc-cancela. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

