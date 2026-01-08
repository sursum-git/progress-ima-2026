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

DEF VAR c-form-epl AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl AS CHAR FORMAT "x(50)".
DEF VAR i-ct       AS INT.
DEF VAR c-desc-item  AS CHAR FORMAT "x(36)".
DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
DEF VAR v-defeito    AS CHAR EXTENT 3.
DEF VAR i-lote AS INT.
DEF VAR c-comando AS CHAR.
DEF VAR c-code-ant AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-imprime fi-num-etiqueta bt-sair 
&Scoped-Define DISPLAYED-OBJECTS fi-num-etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-f-dv.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.75 TOOLTIP "Imprime Etiqueta".

DEFINE BUTTON bt-sair AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.75 TOOLTIP "Sair".

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(9)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 10 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bt-imprime AT ROW 2.71 COL 55.72
     fi-num-etiqueta AT ROW 2.75 COL 27 COLON-ALIGNED NO-LABEL
     bt-sair AT ROW 2.75 COL 62.57
     "Etiqueta:" VIEW-AS TEXT
          SIZE 17 BY 1.5 AT ROW 2.88 COL 11.43
          FONT 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 6.08
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
         TITLE              = "Emiss∆o de Etiqueta Final"
         HEIGHT             = 6.08
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Emiss∆o de Etiqueta Final */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Emiss∆o de Etiqueta Final */
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
    FIND ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = INTEGER(fi-num-etiqueta:SCREEN-VALUE)
         USE-INDEX indice4 EXCLUSIVE-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       MESSAGE 'Etiqueta n∆o Cadastrada no Sistema...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    IF ob-etiqueta.quantidade = 0 THEN DO.
       MESSAGE 'Etiqueta n∆o foi Revisada, Confirme com setor Respons†vel'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    FIND ordem-benefic OF ob-etiqueta NO-LOCK NO-ERROR.
    IF NOT AVAIL ordem-benefic THEN DO.
       MESSAGE 'OB n∆o Encontrada no Sistema...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
    IF NOT AVAIL qualid-tecido THEN DO.
       MESSAGE 'Qualidade do Tecido n∆o Encontrada no Sistema...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    RUN pi-valida. 
    RUN pi-etiqueta.

    ASSIGN ob-etiqueta.situacao = 3.

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''.
    APPLY 'entry' TO fi-num-etiqueta.
    RETURN NO-APPLY.
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


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON RETURN OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CHOOSE' TO bt-imprime.
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + "etq-fin.epl"
         c-form-epl = "n:\especificos\etiqueta\form-etq.epl".

  APPLY 'ENTRY' TO fi-num-etiqueta.
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
  DISPLAY fi-num-etiqueta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE bt-imprime fi-num-etiqueta bt-sair 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta C-Win 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN c-desc-item = ITEM.descricao-1 + TRIM(ITEM.descricao-2).

    IF ITEM.tipo-con-est = 4 THEN DO.
       FIND referencia WHERE
            referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

       FIND FIRST ref-item-ext WHERE 
                  ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND 
                  ref-item-ext.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

       IF AVAIL ref-item-ext THEN
          ASSIGN c-desc-item = c-desc-item + " " + referencia.descricao.
    END.

    FIND FIRST item-ext WHERE
               item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

    IF AVAIL item-ext THEN DO.
       FIND FIRST composi WHERE
                  composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

       ASSIGN c-composicao = "".
       IF AVAIL composi THEN DO.
          DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
             ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
          END.
       END.
    END.

    ASSIGN i-ct = 0
           v-defeito = "".
    FOR EACH mov-est-acbd WHERE
             mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
             mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
             mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
             mov-est-acbd.acondic  = ob-etiqueta.acondic AND
             mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
             mov-est-acbd.classif = "LD" NO-LOCK. 

        ASSIGN i-ct = i-ct + 1.
        IF i-ct > 3 THEN LEAVE.
        IF v-defeito[i-ct] = "" THEN DO.
           ASSIGN v-defeito[i-ct] = mov-est-acbd.cod-tipo-def + "   " + mov-est-acbd.cod-defeito.
        END.
    END.

    CASE ob-etiqueta.nr-lote.
        WHEN "RP" THEN ASSIGN i-lote = 1.
        WHEN "PP" THEN ASSIGN i-lote = 2.
        WHEN "RD" THEN ASSIGN i-lote = 3.
        WHEN "PD" THEN ASSIGN i-lote = 4.
    END CASE.

    ASSIGN c-code-ant = TRIM(ob-etiqueta.it-codigo) + 
                        STRING(INT(ob-etiqueta.cod-refer),"9999999") +
                        STRING(ob-etiqueta.quantidade * 10,"9999") +
                        STRING(ob-etiqueta.nr-ob,"99999") + 
                        STRING(ob-etiqueta.nr-sequencia,"999") + 
                        STRING(i-lote,"9").

    OUTPUT TO VALUE(c-prog-epl) APPEND.
       PUT UNFORMATTED 
           "B180,50,1,2,2,5,75,N," '"' c-code-ant '"' SKIP
           "A220,60,0,1,3,4,N," '"' TRIM(ob-etiqueta.it-codigo) '"' SKIP
           "A420,60,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,1,2)) TRIM(SUBSTR(ob-etiqueta.cod-refer,3,4)) '"' SKIP
           "A640,60,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,7,1)) '"' SKIP
           IF ordem-benefic.cor-etiqueta = 99 THEN "LE210,55,480,50" ELSE "" SKIP
           "A220,130,0,2,1,1,N," '"' TRIM(c-desc-item) '"' SKIP
           IF qualid-tecido.impr-tarja THEN "LE210,125,480,30" ELSE "" SKIP
           "A220,200,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-ob,">>>>>9") '"' SKIP
           "A340,200,0,2,1,1,N," '"' IF AVAIL item-ext THEN STRING(item-ext.largura,"9.99") ELSE "" '"' SKIP
           "A430,200,0,2,1,1,N," '"' STRING(ob-etiqueta.quantidade,">>9.99") '"' SKIP
           "A550,200,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-cortes,">9") '"' SKIP
           "A640,190,0,1,3,3,N," '"' TRIM(ob-etiqueta.nuance) '"' SKIP
           IF qualid-tecido.class-qualid = 2 THEN "LE610,170,80,135" ELSE "" SKIP
           "A220,265,0,2,1,1,N," '"' TRIM(c-composicao[1]) '"' SKIP
           "A220,285,0,2,1,1,N," '"' TRIM(c-composicao[2]) '"' SKIP
           "A610,270,0,2,1,1,N," '"' TRIM(ob-etiqueta.acondic) '"' SKIP
           "A240,330,0,4,3,4,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
           "B295,430,0,1,3,7,60,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
           "A620,520,0,2,1,1,N," '"' TRIM(v-defeito[1]) '"' SKIP
           "A620,620,0,2,1,1,N," '"' TRIM(v-defeito[2]) '"' SKIP
           "A620,720,0,2,1,1,N," '"' TRIM(v-defeito[3]) '"' SKIP.

       IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
          CASE item-ext.cod-rlgp:
               WHEN 1 THEN
                  PUT UNFORMATTED
                      "GG260,535," '"imag0204"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0302"' SKIP 
                      "GG425,535," '"imag0402"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
               WHEN 2 THEN
                  PUT UNFORMATTED
                      "GG260,535," '"imag0204"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0303"' SKIP 
                      "GG425,535," '"imag0402"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
               WHEN 3 THEN 
                  PUT UNFORMATTED
                      "GG260,535," '"imag0206"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0303"' SKIP 
                      "GG425,535," '"imag0402"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
               WHEN 4 THEN                           
                  PUT UNFORMATTED
                      "GG260,535," '"imag0203"' SKIP
                      "GG310,530," '"imag0102"' SKIP 
                      "GG370,530," '"imag0301"' SKIP 
                      "GG425,535," '"imag0503"' SKIP 
                      "GG480,530," '"imag0604"' SKIP. 
          END CASE.
       END.

       PUT UNFORMATTED
           "P1" SKIP.
    OUTPUT CLOSE.
    
    MESSAGE "Vou Imprimir"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

