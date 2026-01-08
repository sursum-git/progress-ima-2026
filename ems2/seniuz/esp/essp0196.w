&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-form-epl    AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl    AS CHAR FORMAT "x(50)".
DEF VAR i-tot-etq     AS INT.
DEF VAR i-etq-imp     AS INT.
DEF VAR c-seq         AS CHAR.
DEF VAR c-origem      AS CHAR.
DEF VAR c-emissao     AS CHAR.
DEF VAR c-pedido      AS CHAR.
DEF VAR c-localiz     AS CHAR. 
DEF VAR c-desc-item   AS CHAR FORMAT "x(40)".
DEF VAR c-comando     AS CHAR.
DEF VAR l-erro        AS LOG.
DEF VAR i-digito      AS INT NO-UNDO.
DEF VAR c-cod-estabel LIKE nota-fiscal.cod-estabel.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nr-nota-fis fi-num-etiqueta bt-imprime ~
bt-sair 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-nota-fis fi-volumes fi-nome-abrev ~
fi-num-etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Btn 2" 
     SIZE 19 BY 1.75 TOOLTIP "Imprime Etiqueta".

DEFINE BUTTON bt-sair AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-exi.bmp":U
     LABEL "Btn 2" 
     SIZE 13 BY 1.75 TOOLTIP "Sair".

DEFINE VARIABLE fi-nome-abrev AS CHARACTER FORMAT "x(23)":U 
     VIEW-AS FILL-IN 
     SIZE 59.72 BY 1.75
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis AS CHARACTER FORMAT "x(9)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(10)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-volumes AS CHARACTER FORMAT "999999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.75
     FONT 10 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi-nr-nota-fis AT ROW 1.17 COL 29.86 COLON-ALIGNED NO-LABEL
     fi-volumes AT ROW 5.17 COL 29.86 COLON-ALIGNED NO-LABEL
     fi-nome-abrev AT ROW 7.17 COL 29.86 COLON-ALIGNED NO-LABEL
     fi-num-etiqueta AT ROW 3.17 COL 29.86 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     bt-imprime AT ROW 9.54 COL 2.72
     bt-sair AT ROW 9.63 COL 78.43
     "Etiqueta (QTD):" VIEW-AS TEXT
          SIZE 29.14 BY 1.5 AT ROW 5.29 COL 2.14 WIDGET-ID 2
          FONT 10
     "Nota Fiscal:" VIEW-AS TEXT
          SIZE 22 BY 1.5 AT ROW 1.29 COL 9.29
          FONT 10
     "Cliente:" VIEW-AS TEXT
          SIZE 14.14 BY 1.5 AT ROW 7.33 COL 17
          FONT 10
     "Etiqueta:" VIEW-AS TEXT
          SIZE 16 BY 1.5 AT ROW 3.13 COL 15.14 WIDGET-ID 6
          FONT 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.72 BY 10.5
         CANCEL-BUTTON bt-sair.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Emiss∆o de Etiquetas para Exportaá∆o"
         HEIGHT             = 10.54
         WIDTH              = 93.14
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 93.14
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 93.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-nome-abrev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-volumes IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Emiss∆o de Etiquetas para Exportaá∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Emiss∆o de Etiquetas para Exportaá∆o */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
  FIND nota-fiscal WHERE
       nota-fiscal.cod-estabel = c-cod-estabel AND
       nota-fiscal.serie       = para-fat.serie-pad AND
       nota-fiscal.nr-nota-fis = STRING(INT(fi-nr-nota-fis:SCREEN-VALUE),"9999999")
       NO-LOCK NO-ERROR.

  IF NOT AVAIL nota-fiscal THEN DO.
     MESSAGE 'Nota Fiscal n∆o Cadastrada no Sistema...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.

  FIND emitente WHERE
       emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.

  IF emitente.natureza <> 3 THEN DO.
     MESSAGE 'Nota Fiscal n∆o Ç para Exportaá∆o,' SKIP
             'Imposs°vel emitir Etiquetas...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.

  IF fi-num-etiqueta:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Deseja Imprimir Todas as Etiquetas da Nota Fiscal " + nota-fiscal.nr-nota-fis SKIP(1) 
              "Confirma a Impress∆o das Etiquetas ?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.

      IF NOT l-conf THEN DO.
         APPLY 'entry' TO fi-num-etiqueta.
         RETURN NO-APPLY.
      END.

     FOR EACH ped-item-res WHERE
              ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
              ped-item-res.serie       = nota-fiscal.serie AND
              ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
              ped-item-res.faturado    = YES NO-LOCK
              BREAK BY STRING(ped-item-res.volume-ini) + 
                       STRING(ped-item-res.volume-fim).

          IF ped-item-res.lote <> '' AND
             LOOKUP(SUBSTR(ped-item-res.sigla-emb,1,1),'P,F') > 0 
             THEN NEXT.

          FOR EACH ped-item-rom WHERE
                   ped-item-rom.cod-estabel = ped-item-res.cod-estabel AND
                   ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                   ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                   ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia 
                   NO-LOCK.

              FIND ob-etiqueta where
                   ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                   ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta NO-LOCK NO-ERROR.
              IF NOT AVAIL ob-etiqueta THEN NEXT.
           /*   MESSAGE "Vou Imprimir a Etiqueta " ob-etiqueta.num-etiqueta
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */  

              RUN esapi/imp-etq-export.p (INPUT ob-etiqueta.num-etiqueta,
                                          INPUT NO).
              
          END.
     END.
     
     ASSIGN fi-num-etiqueta:SCREEN-VALUE   = ""
            fi-nr-nota-fis:SCREEN-VALUE = ""
            fi-volumes:SCREEN-VALUE        = ""
            fi-nome-abrev:SCREEN-VALUE     = "".

     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
     
  END.
  ELSE DO:
     FIND ob-etiqueta WHERE
          ob-etiqueta.cod-estabel  = c-cod-estabel AND
          ob-etiqueta.num-etiqueta = INTEGER(fi-num-etiqueta:SCREEN-VALUE) 
          USE-INDEX indice4 SHARE-LOCK NO-ERROR.

     IF NOT AVAIL ob-etiqueta THEN DO.
        MESSAGE 'Etiqueta n∆o Cadastrada no Sistema...'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'entry' TO fi-num-etiqueta.
        RETURN NO-APPLY.
     END.

     FIND ped-item-rom WHERE
          ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
          ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
     IF NOT AVAIL ped-item-rom THEN DO.
        MESSAGE 'Etiqueta n∆o possui Detalhe da Reserva Cadastrada no Sistema' SKIP
                'Impossivel emitir etiqueta...'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'entry' TO fi-num-etiqueta.
        RETURN NO-APPLY.
     END.

     FIND FIRST ped-item-res WHERE
                ped-item-res.cod-estabel  = ped-item-rom.cod-estabel AND
                ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
                ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
                ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
     IF NOT AVAIL ped-item-res THEN DO:
        MESSAGE 'Etiqueta n∆o possui Reservas Cadastrada no Sistema' SKIP
                'Impossivel emitir etiqueta...'
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        APPLY 'entry' TO fi-num-etiqueta.
        RETURN NO-APPLY.
     END.

     IF ped-item-res.nr-nota-fis <> DEC(fi-nr-nota-fis:SCREEN-VALUE) THEN DO:
        MESSAGE 'Etiqueta,  pertence a Nota Fiscal ' + STRING(ped-item-res.nr-nota-fis) SKIP 
                'Impossivel emitir etiqueta...'
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO fi-num-etiqueta.
         RETURN NO-APPLY.
     END.

     /*
     IF ped-item-rom.marca <> '' AND c-seg-usuario <> 'super' THEN DO. /* Etiqueta Impressa */
        RUN pi-valida. 
        IF RETURN-VALUE = 'NOK' THEN DO.
           APPLY 'entry' TO fi-num-etiqueta.
           RETURN NO-APPLY.
        END.
     END.
     */
     RUN esapi/imp-etq-export.p (INPUT ob-etiqueta.num-etiqueta, 
                                 INPUT NO). 
     ASSIGN fi-num-etiqueta:SCREEN-VALUE   = "".

     APPLY 'entry' TO fi-num-etiqueta.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair C-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-nota-fis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis C-Win
ON LEAVE OF fi-nr-nota-fis IN FRAME DEFAULT-FRAME
DO:
  ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-volumes:SCREEN-VALUE    = ""
         fi-nome-abrev:SCREEN-VALUE = "".

  FIND nota-fiscal WHERE
       nota-fiscal.cod-estabel = c-cod-estabel  AND
       nota-fiscal.serie       = para-fat.serie-pad AND
       nota-fiscal.nr-nota-fis = STRING(INT(fi-nr-nota-fis:SCREEN-VALUE),"9999999")
       NO-LOCK NO-ERROR.

  IF NOT AVAIL nota-fiscal THEN DO.
     MESSAGE 'Nota Fiscal n∆o Cadastrada no Sistema...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.

  FIND emitente WHERE
       emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
  
  IF emitente.natureza <> 3 THEN DO.
     MESSAGE 'Nota Fiscal n∆o Ç para Exportaá∆o,' SKIP
             'Imposs°vel emitir Etiquetas...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.
  
  ASSIGN i-tot-etq = 0
         i-etq-imp = 0.
  FOR EACH ped-item-res WHERE
           ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
           ped-item-res.serie       = nota-fiscal.serie AND
           ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
           ped-item-res.faturado    = YES NO-LOCK
           BREAK BY STRING(ped-item-res.volume-ini) + 
                    STRING(ped-item-res.volume-fim).

       IF ped-item-res.lote <> '' AND
          LOOKUP(SUBSTR(ped-item-res.sigla-emb,1,1),'P,F') > 0 
          THEN NEXT.

       FOR EACH ped-item-rom WHERE
                ped-item-rom.cod-estabel  = ped-item-res.cod-estabel AND
                ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia 
                NO-LOCK.

           ASSIGN i-tot-etq = i-tot-etq + 1.
       END.
  END.
  ASSIGN fi-volumes:SCREEN-VALUE = STRING(i-tot-etq,">999")
         fi-nome-abrev:SCREEN-VALUE = emitente.nome-emit.
         bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis C-Win
ON RETURN OF fi-nr-nota-fis IN FRAME DEFAULT-FRAME
DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON LEAVE OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
  APPLY 'CHOOSE' TO bt-imprime.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON RETURN OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
  IF SELF:SCREEN-VALUE <> '' THEN DO:
     APPLY 'TAB' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON VALUE-CHANGED OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
  
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
     SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
     BELL.
     APPLY 'backspace' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN i-digito = 99.
  IF LENGTH(SELF:SCREEN-VALUE) = 10 THEN DO.
     ASSIGN i-digito = INT(SUBSTRING(SELF:SCREEN-VALUE,10,1)).
     ASSIGN SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,9).
     APPLY 'END' TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

FIND FIRST para-fat NO-LOCK NO-ERROR.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,
                                 OUTPUT c-cod-estabel).
  IF c-cod-estabel = "" THEN DO:
     MESSAGE "Usu†rio: " + c-seg-usuario + " n∆o cadastrado no parÉmetro de permiss‰es."
         VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.
  IF c-cod-estabel = '' OR c-cod-estabel = '0' THEN
     ASSIGN c-cod-estabel = '1'.

  IF c-cod-estabel = '1' THEN /* Paraopeba */
     ASSIGN c-form-epl = "n:\especificos\etiqueta\form-nfpar.epl".

  ASSIGN bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO. 

  APPLY 'ENTRY' TO fi-nr-nota-fis.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-nota-fis fi-volumes fi-nome-abrev fi-num-etiqueta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fi-nr-nota-fis fi-num-etiqueta bt-imprime bt-sair 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida C-Win 
PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR c-ok AS CHAR.
 DEF VAR c-usuarios AS CHAR.

 FOR EACH usuar_grp_usuar WHERE 
          usuar_grp_usuar.cod_grp_usuar = "q01" NO-LOCK.
     ASSIGN c-usuarios = c-usuarios + usuar_grp_usuar.cod_usuar + ','.
 END.

 RUN btb/btb910zc.p (INPUT c-usuarios, INPUT YES, INPUT YES, OUTPUT c-ok).
 IF c-ok = ? THEN 
    RETURN 'NOK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

