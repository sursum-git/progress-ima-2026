&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR i-ct AS INT.
DEF VAR i-num-bar     AS INT.
DEF VAR c-form-epl    AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl    AS CHAR FORMAT "x(50)".
DEF VAR c-pedido      AS CHAR.
DEF VAR c-volume      AS CHAR.
DEF VAR c-comando     AS CHAR.
DEF VAR c-impressora  AS CHAR.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom RECT-1 fi-qt-fardos bt-ok ~
bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-etq fi-qt-fardos tg-etq-fardo ~
tg-etq-exped 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-qt-fardos AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-etq AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .88
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 7.92.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 51.29 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-etq-exped AS LOGICAL INITIAL no 
     LABEL "Re-Imprimir Etiquetas com Dados da Expediá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .83 NO-UNDO.

DEFINE VARIABLE tg-etq-fardo AS LOGICAL INITIAL no 
     LABEL "Criar / Imprimir Etiquetas de Fardo" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.86 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-tot-etq AT ROW 2 COL 9.14 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fi-qt-fardos AT ROW 4.25 COL 9.14 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     tg-etq-fardo AT ROW 5.88 COL 11.14 WIDGET-ID 10
     tg-etq-exped AT ROW 6.88 COL 11.14 WIDGET-ID 16
     bt-ok AT ROW 9.46 COL 3
     bt-cancela AT ROW 9.46 COL 13.57
     bt-ajuda AT ROW 9.46 COL 42.29 WIDGET-ID 8
     "Qtde de Fardos / Volumes" VIEW-AS TEXT
          SIZE 23 BY .54 AT ROW 3.67 COL 11.14 WIDGET-ID 18
     "Total de Etiquetas Separadas" VIEW-AS TEXT
          SIZE 27 BY .54 AT ROW 1.38 COL 11.14 WIDGET-ID 20
     rt-buttom AT ROW 9.25 COL 2
     RECT-1 AT ROW 1.08 COL 2 WIDGET-ID 4
     SPACE(0.71) SKIP(1.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Fardos / Volumes - ESSP0154D"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

ASSIGN 
       bt-ajuda:HIDDEN IN FRAME D-Dialog           = TRUE.

ASSIGN 
       bt-cancela:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-etq IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-etq-exped IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-etq-fardo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Fardos / Volumes - ESSP0154D */
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


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-qt-fardos.

   IF fi-qt-fardos = 0 THEN DO.
      MESSAGE 'Favor Informar a Quantidade de Fardos / Volumes'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO fi-qt-fardos.
      RETURN NO-APPLY.
   END.

   IF fi-qt-fardos > fi-tot-etq THEN DO.
      MESSAGE 'Quantidade de Fardos inv†lida...' SKIP
              'est† maior que a quantidade de Etiquetas...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO fi-qt-fardos.
      RETURN NO-APPLY.
   END.

   FIND ped-venda WHERE
        ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.

   FIND ped-venda-ext WHERE
        ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
        ped-venda-ext.nr-pedido = ped-venda.nr-pedido
        SHARE-LOCK NO-ERROR.
   IF NOT AVAIL ped-venda-ext THEN DO.
      MESSAGE 'Erro de Consistància, Informaá‰es de Fardo n∆o ser∆o Gravadas..'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO fi-qt-fardos.
      RETURN NO-APPLY.
   END.
   ASSIGN ped-venda-ext.qt-fardos = fi-qt-fardos.

   FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.

   IF INPUT FRAME {&FRAME-NAME} tg-etq-fardo THEN DO.
      DO i-ct = 1 TO INPUT FRAME {&FRAME-NAME} fi-qt-fardos.
         CREATE ob-etiqueta.
         ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq-med)
                ob-etiqueta.cod-estabel = ped-venda.cod-estabel
                ob-etiqueta.situacao = 5  /* Faturada */
                ob-etiqueta.nr-lote   = 'FD'                                  
                ob-etiqueta.ob-origem = ped-venda.nr-pedcli
                ob-etiqueta.dt-emissao = TODAY
                ob-etiqueta.hr-emissao = STRING(TIME,"HH:MM")
                ob-etiqueta.resp-revisao = c-seg-usuario.
    
         RUN pi-etiqueta.
      END.
   END.

   IF INPUT FRAME {&FRAME-NAME} tg-etq-exped THEN DO.
      ASSIGN i-ct = 0.
      FOR EACH ped-item-rom WHERE
               ped-item-rom.nr-pedcli = ped-venda.nr-pedcli AND
               ped-item-rom.nome-abrev = ped-venda.nome-abrev NO-LOCK.
          FIND ob-etiqueta WHERE
               ob-etiqueta.cod-estabel = ped-venda.cod-estabel AND
               ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
               NO-LOCK NO-ERROR.
          IF NOT AVAIL ob-etiqueta THEN
             FIND ob-etiqueta WHERE
                  ob-etiqueta.cod-estabel  = '504' AND
                  ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                  NO-LOCK NO-ERROR.

          IF AVAIL ob-etiqueta THEN DO.
             ASSIGN i-ct = i-ct + 1.
             RUN pi-etiqueta.
          END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qt-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-fardos D-Dialog
ON LEAVE OF fi-qt-fardos IN FRAME D-Dialog
DO:
    ASSIGN tg-etq-fardo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           tg-etq-fardo:SCREEN-VALUE = 'NO'
           tg-etq-exped:SENSITIVE = NO
           tg-etq-exped:SCREEN-VALUE = 'NO'.

  IF INPUT FRAME {&FRAME-NAME} fi-qt-fardos < INPUT FRAME {&FRAME-NAME} fi-tot-etq  THEN
     ASSIGN tg-etq-fardo:SENSITIVE = YES
            tg-etq-fardo:SCREEN-VALUE = 'YES'.
  ELSE
     ASSIGN tg-etq-exped:SENSITIVE = YES
            tg-etq-exped:SCREEN-VALUE = 'YES'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fi-tot-etq fi-qt-fardos tg-etq-fardo tg-etq-exped 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom RECT-1 fi-qt-fardos bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
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

  FIND ped-venda WHERE
       ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.

  FIND ped-venda-ext WHERE
       ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
       ped-venda-ext.nr-pedido = ped-venda.nr-pedido
       NO-LOCK NO-ERROR.
  IF NOT AVAIL ped-venda-ext THEN DO.
     MESSAGE 'Erro de Consistància, Informaá‰es de Fardo n∆o ser∆o Gravadas..'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-qt-fardos = ped-venda-ext.qt-fardos.

  FOR EACH ped-item-rom WHERE
           ped-item-rom.nr-pedcli = ped-venda.nr-pedcli AND
           ped-item-rom.nome-abrev = ped-venda.nome-abrev NO-LOCK.
      ASSIGN fi-tot-etq = fi-tot-etq + 1.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta D-Dialog 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".epl".

    IF ped-venda.cod-estabel = '1' THEN
       ASSIGN c-form-epl = SEARCH("etiqueta\form-fardo-ima.epl").
    ELSE
       ASSIGN c-form-epl = SEARCH("etiqueta\form-fardo-med.epl").

    OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

    IF ped-venda.nome-abrev-tri <> '' THEN
       FIND emitente WHERE
            emitente.nome-abrev = ped-venda.nome-abrev-tri NO-LOCK NO-ERROR.
    ELSE
       FIND emitente WHERE
            emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    ASSIGN c-pedido = "Pedido: " + ped-venda.nr-pedcli.

    FIND transporte WHERE
         transporte.nome-abrev = ped-venda.nome-transp NO-LOCK NO-ERROR.

    ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

    ASSIGN c-volume = STRING(i-ct) + "/" + STRING(fi-qt-fardos). 

    OUTPUT TO VALUE(c-prog-epl) APPEND. 
       PUT UNFORMATTED 
           "A30,165,0,1,1,3,N," '"' TRIM(emitente.nome-emit) '"' SKIP
           "A120,219,0,1,1,2,N," '"' TRIM(emitente.cidade) '"' SKIP 
           "A610,219,0,1,1,2,N," '"' TRIM(emitente.estado) '"' SKIP 
           "A120,260,0,1,2,4,N," '"' TRIM(ped-venda.nr-pedcli) '"' SKIP 
           "A310,275,0,1,1,2,N," '"' TRIM(transporte.nome) '"' SKIP 
           "A170,340,0,1,2,4,N," '"' STRING(c-volume) '"' SKIP 
           "A170,400,0,1,2,4,N," '"' STRING(fi-tot-etq) '"' SKIP 
           "B415,320,0,1,3,7,70,N," '"' STRING(i-num-bar,"9999999999") '"' SKIP
           "A370,395,0,3,3,4,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP.

       PUT UNFORMATTED
           "P1" SKIP.
    OUTPUT CLOSE.

     FIND imprsor_usuar WHERE 
          imprsor_usuar.nom_impressora = "rabbit" AND 
          imprsor_usuar.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

    IF AVAIL imprsor_usuar THEN DO:
       ASSIGN c-impressora = imprsor_usuar.nom_disposit_so
              c-comando = "copy /Y /b " + c-prog-epl + " " + c-impressora. 
       
       OS-COMMAND SILENT VALUE(c-comando).
    END.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

