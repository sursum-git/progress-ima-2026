&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0176 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */

/* Temp Table Definitions ---             */

DEFINE TEMP-TABLE tt-baixa 
       FIELD nr-mistura     AS INT
       FIELD codigo         AS CHAR 
       FIELD Padrao         AS CHAR
       FIELD desc-coloracao AS CHAR 
       FIELD desc-tipo      AS CHAR
       FIELD Prog           AS INT
       FIELD qtd            AS INT 
       FIELD peso           AS DEC
       INDEX indice1 nr-mistura codigo padrao.

DEFINE TEMP-TABLE tt-mistura LIKE mp-mistura. 

DEFINE BUFFER b-tt-baixa FOR tt-baixa.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR i-lin         AS INT.
DEF VAR i-pag         AS INT.
DEF VAR de-ger-prog   AS INT.
DEF VAR de-ger-realiz AS INT.
DEF VAR de-tot-realiz AS INT.
DEF VAR de-ger-peso   AS DEC.

/* Variaveis da Rotina de ImpressÆo */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.



/* Variavies de Parƒmetros */
DEFINE VAR c-dt-baixa  AS CHAR.
DEFINE VAR l-ok        AS LOG.

/* Variaveis Usadas Na Gera‡Æo da Planilha Excel */
DEFINE VAR c-empresa LIKE empresa.razao-social.
DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR      FORMAT "x(100)".
DEFINE VAR arq-saida   AS CHAR      FORMAT "x(45)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-baixa

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-baixa tt-mistura

/* Definitions for BROWSE br-baixa                                      */
&Scoped-define FIELDS-IN-QUERY-br-baixa tt-baixa.codigo tt-baixa.padrao tt-baixa.desc-coloracao tt-baixa.desc-tipo tt-baixa.prog tt-baixa.qtd tt-baixa.peso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-baixa   
&Scoped-define SELF-NAME br-baixa
&Scoped-define OPEN-QUERY-br-baixa RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-baixa WHERE                                  tt-baixa.nr-mistura = tt-mistura.nr-mistura                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-baixa tt-baixa
&Scoped-define FIRST-TABLE-IN-QUERY-br-baixa tt-baixa


/* Definitions for BROWSE br-mistura                                    */
&Scoped-define FIELDS-IN-QUERY-br-mistura tt-mistura.nr-mistura tt-mistura.dt-mistura tt-mistura.tp-mistura tt-mistura.qtd-fardos tt-mistura.qtd-bancadas   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-mistura   
&Scoped-define SELF-NAME br-mistura
&Scoped-define QUERY-STRING-br-mistura FOR EACH tt-mistura NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-mistura OPEN QUERY {&SELF-NAME} FOR EACH tt-mistura NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-mistura tt-mistura
&Scoped-define FIRST-TABLE-IN-QUERY-br-mistura tt-mistura


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-baixa}~
    ~{&OPEN-QUERY-br-mistura}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-mistura bt-param bt-imprime ~
bt-excel br-baixa bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-prog fi-tot-realiz fi-tot-peso 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 4.86 BY 1.29 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.29.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir a Carteira de Pedidos Selecionados".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "Parƒmetros"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-tot-peso AS DECIMAL FORMAT "      ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-prog AS INTEGER FORMAT "  ZZZ,ZZZ,ZZ9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi-tot-realiz AS INTEGER FORMAT " ZZZ,ZZZ,ZZ9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 21
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-baixa FOR 
      tt-baixa SCROLLING.

DEFINE QUERY br-mistura FOR 
      tt-mistura SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-baixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-baixa C-Win _FREEFORM
  QUERY br-baixa NO-LOCK DISPLAY
      tt-baixa.codigo         COLUMN-LABEL "Codigo"      WIDTH 6
      tt-baixa.padrao         COLUMN-LABEL "Padrao"     FORMAT "x(12)" WIDTH 15.00
      tt-baixa.desc-coloracao COLUMN-LABEL "Tonalidade" FORMAT "x(15)" WIDTH 15.00
      tt-baixa.desc-tipo      COLUMN-LABEL "Tipo"       FORMAT "x(10)" WIDTH 8
      tt-baixa.prog           COLUMN-LABEL "Programado" 
      tt-baixa.qtd            COLUMN-LABEL "Realizado"
      tt-baixa.peso           COLUMN-LABEL "Peso Fardos" WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 14.21
         FONT 1
         TITLE "Posi‡Æo Analitica da Baixa" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-mistura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-mistura C-Win _FREEFORM
  QUERY br-mistura NO-LOCK DISPLAY
      tt-mistura.nr-mistura   COLUMN-LABEL "N§ Mistura"      WIDTH 10.0
      tt-mistura.dt-mistura   COLUMN-LABEL "Data Mistura"    WIDTH 12.0
      tt-mistura.tp-mistura   COLUMN-LABEL "Tipo Mistura"    WIDTH 28.0
      tt-mistura.qtd-fardos   COLUMN-LABEL "N§ Fardos"       WIDTH 10.0
      tt-mistura.qtd-bancadas COLUMN-LABEL "N§ Bancadas"     WIDTH 12.0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 5
         FONT 1
         TITLE "Misturas do Dia" ROW-HEIGHT-CHARS .79.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-mistura AT ROW 1.17 COL 1
     bt-param AT ROW 1.29 COL 81
     bt-imprime AT ROW 2.75 COL 81
     bt-excel AT ROW 4.25 COL 81
     br-baixa AT ROW 6.54 COL 1
     bt-exit AT ROW 18.96 COL 81
     bt-ajuda AT ROW 20.29 COL 81
     fi-tot-prog AT ROW 20.96 COL 45.14 COLON-ALIGNED NO-LABEL
     fi-tot-realiz AT ROW 20.96 COL 54.29 COLON-ALIGNED NO-LABEL
     fi-tot-peso AT ROW 20.96 COL 62.14 COLON-ALIGNED NO-LABEL
     rt-buttom AT ROW 1 COL 80
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 1
         SIZE 86.43 BY 21.08
         FONT 1.


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
         TITLE              = "Analise da Baixa Fardos - ESSP0176"
         COLUMN             = 21.43
         ROW                = 7.29
         HEIGHT             = 21.21
         WIDTH              = 87.43
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
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
   FRAME-NAME                                                           */
/* BROWSE-TAB br-mistura rt-buttom DEFAULT-FRAME */
/* BROWSE-TAB br-baixa bt-excel DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-peso IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prog IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-realiz IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-baixa
/* Query rebuild information for BROWSE br-baixa
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-baixa WHERE
                                 tt-baixa.nr-mistura = tt-mistura.nr-mistura
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-baixa */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-mistura
/* Query rebuild information for BROWSE br-mistura
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mistura NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-mistura */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Analise da Baixa Fardos - ESSP0176 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Analise da Baixa Fardos - ESSP0176 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-baixa
&Scoped-define SELF-NAME br-baixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-baixa C-Win
ON ROW-DISPLAY OF br-baixa IN FRAME DEFAULT-FRAME /* Posi‡Æo Analitica da Baixa */
DO:
   IF tt-baixa.prog >  0 THEN
      RUN pi-cor (INPUT 12).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-mistura
&Scoped-define SELF-NAME br-mistura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-mistura C-Win
ON VALUE-CHANGED OF br-mistura IN FRAME DEFAULT-FRAME /* Misturas do Dia */
DO:
   APPLY 'value-changed' TO br-baixa IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-baixa IN FRAME {&FRAME-NAME}.
   {&OPEN-QUERY-br-baixa}
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME DEFAULT-FRAME /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
    RUN esdlg/d01essp0176.w (OUTPUT arq-saida).
    IF arq-saida <> "" THEN DO:
       RUN pi-gera-excel (INPUT arq-saida).
       MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
               "Para acess -lo,  abra-o atrav‚s do Excel."
           VIEW-AS ALERT-BOX INFO BUTTONS OK. 
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0176a.w (INPUT-OUTPUT c-dt-baixa,
                        INPUT-OUTPUT l-ok).                  
   ASSIGN c-win:SENSITIVE = YES.

   IF l-ok THEN                                     
      RUN pi-popula-browse.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-baixa
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
/*
ON 'F5':U OF br-ob-etiquetas DO:
   {&OPEN-QUERY-br-ob-etiquetas}
   APPLY 'value-changed' TO br-ob-etiquetas.
END.

ASSIGN h-query = br-ob-etiquetas:QUERY.
br-ob-etiquetas:NUM-LOCKED-COLUMNS = 3.
*/

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   ASSIGN c-dt-baixa = STRING(TODAY).

   APPLY 'choose' TO bt-param.

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
  DISPLAY fi-tot-prog fi-tot-realiz fi-tot-peso 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom br-mistura bt-param bt-imprime bt-excel br-baixa bt-exit 
         bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-arquivo AS CHAR.

 DEF VAR h-prog AS HANDLE NO-UNDO.
 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN EXECUTE IN h-prog(INPUT "EXCEL.EXE", INPUT p-arquivo).

 DELETE PROCEDURE h-prog.
 PAUSE 5 NO-MESSAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor C-Win 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-cor AS INT.

 tt-baixa.codigo:FGCOLOR IN BROWSE {&browse-name}         = p-cor.
 tt-baixa.padrao:FGCOLOR IN BROWSE {&browse-name}         = p-cor.
 tt-baixa.desc-coloracao:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-baixa.desc-tipo:FGCOLOR IN BROWSE {&browse-name}      = p-cor.
 tt-baixa.prog:FGCOLOR IN BROWSE {&browse-name}           = p-cor.
 tt-baixa.qtd:FGCOLOR IN BROWSE {&browse-name}            = 1.
 tt-baixa.peso:FGCOLOR IN BROWSE {&browse-name}           = 1.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
 HIDE FRAME f-main.

 DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
 ENABLE ALL WITH FRAME frm_excel.
    
 RUN pi-abre-excel (INPUT "").
 PAUSE 3 NO-MESSAGE.

 DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
 DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

 RUN pi-monta-planilha.

 OS-DELETE VALUE(p-arq-saida).
 DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    
 DDE EXECUTE   sys COMMAND "[close(0)]". 
 DDE EXECUTE   sys COMMAND "[quit()]". 
   
 DDE TERMINATE sys.
    
 HIDE FRAME frm_excel.
 CLEAR FRAME frm_excel.
 DISABLE ALL WITH FRAME frm_excel.

 VIEW FRAME f-main.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  46
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  52
        "HORA: "                                  AT  70
        STRING(TIME,"hh:mm:ss")                   AT  76
        "PAG:"                                    AT 101
        i-pag FORMAT ">>>"                        AT 106
        SKIP(1).

    PUT "RELATORIO DA POSICAO ANALITICA DA BAIXA FARDOS DO DIA" AT 19
        c-dt-baixa  FORMAT "x(10)" AT 73 SKIP(1).

    PUT "MISTURA CODIGO PADRAO               TONALIDADE      TIPO            PROGRAMADO  REALIZADO    %    PESO FARDO" AT 1.
    PUT "------- ------ -------------------- --------------- ------------    ----------  --------- ------ -----------" AT 1.
    ASSIGN i-pag = i-pag + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
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
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
         PUT CONTROL "~033E~033(s15H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0176.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-lin         = 99
           i-pag         =  1
           de-ger-prog   =  0
           de-ger-realiz =  0
           de-ger-peso   =  0.

    FOR EACH tt-baixa  NO-LOCK 
             BREAK BY tt-baixa.nr-mistura 
                   BY tt-baixa.codigo
                   BY tt-baixa.padrao. 

        IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.

        IF FIRST-OF(tt-baixa.nr-mistura) THEN DO:
           de-tot-realiz = 0.
           FOR EACH b-tt-baixa WHERE
                    b-tt-baixa.nr-mistura = tt-baixa.nr-mistura NO-LOCK.
               ASSIGN de-tot-realiz = de-tot-realiz + b-tt-baixa.qtd.
           END.
           PUT tt-baixa.nr-mistura     FORMAT ">>>,>>9"   AT  1.
        END.

        PUT tt-baixa.codigo         FORMAT "x(3)"        AT  9
            tt-baixa.padrao         FORMAT "x(19)"       AT 16
            tt-baixa.desc-coloracao FORMAT "x(14)"       AT 37
            tt-baixa.desc-tipo      FORMAT "X(11)"       AT 53
            tt-baixa.prog           FORMAT ">,>>>,>>>"   AT 69
            tt-baixa.qtd            FORMAT ">,>>>,>>>"   AT 81
            (tt-baixa.qtd / de-tot-realiz) * 100 FORMAT ">>9.99" AT 91
            tt-baixa.peso           FORMAT ">>>>,>>9.99" AT 98.

        ACCUMULATE tt-baixa.prog (TOTAL BY tt-baixa.nr-mistura).
        ACCUMULATE tt-baixa.qtd  (TOTAL BY tt-baixa.nr-mistura).
        ACCUMULATE tt-baixa.peso (TOTAL BY tt-baixa.nr-mistura).

        IF LAST-OF(tt-baixa.nr-mistura)  THEN DO:
           ASSIGN de-ger-prog   = de-ger-prog   + ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.prog
                  de-ger-realiz = de-ger-realiz + ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.qtd
                  de-ger-peso   = de-ger-peso   + ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.peso.
           PUT SKIP.
           PUT "-----------  ---------        -----------" AT 68 SKIP.
           PUT "TOTAL......:" AT 53.
           PUT (ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.prog) FORMAT ">,>>>,>>>"   AT 69.
           PUT (ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.qtd)  FORMAT ">,>>>,>>>"   AT 81. 
           PUT (ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.peso) FORMAT ">>>>,>>9.99" AT 98 SKIP(1). 
        END.
    END.
    IF (ACCUM TOTAL tt-baixa.peso) <> 0 THEN DO:
       PUT "-----------  ---------        -----------" AT 68 SKIP.
       PUT "TOTAL GERAL:" AT 53.
       PUT de-ger-prog   FORMAT ">,>>>,>>>"   AT 69.
       PUT de-ger-realiz FORMAT ">,>>>,>>>"   AT 81.
       PUT de-ger-peso   FORMAT ">>>>,>>9.99" AT 98.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha C-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lin-col AS CHAR.

 ASSIGN de-ger-prog   = 0
        de-ger-realiz = 0
        de-ger-peso   = 0.

 /* Cabe‡alho  da Planilha */
 ASSIGN c-Lin = c-empresa + "             " + " POSI€ÇO ANALÖTICA DA BAIXA FARDOS DO DIA: "  + c-dt-baixa. 
 DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
 DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C8")]'.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".


/*
 ASSIGN i-Lin = 3.
 FOR EACH tt-mistura NO-LOCK.
     ASSIGN c-Lin = " N§ MISTURA: "  + TRIM(STRING(tt-mistura.nr-mistura, ">>>>>>9")) +
                    "        DATA: " + STRING(tt-mistura.dt-mistura, "99/99/9999")    +
                    "        TIPO: " + tt-mistura.tp-mistura.
     DDE SEND i-canal SOURCE c-lin ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C7")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
     DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
     ASSIGN i-lin = i-lin + 1.
 END.
 IF i-lin <> 3 THEN
    ASSIGN i-lin = i-lin + 1.
*/
 
 
 /* Cabe‡alho dos Dados */
 ASSIGN i-lin = 3.
 DDE SEND i-canal SOURCE "MISTURA"    ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE "CODIGO"     ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE "PADRAO"     ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE "TONALIDADE" ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE "TIPO"       ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE "PROGRAMADO" ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE "REALIZADO"  ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 DDE SEND i-canal SOURCE "   %  "     ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
 DDE SEND i-canal SOURCE "PESO FARDO" ITEM "L" + TRIM(STRING(i-Lin)) + "C9".

 DDE EXECUTE i-canal COMMAND '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C9")]'.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(8.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(7.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(20.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(25.00)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(6.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

  /* Montagem das Celulas de Dados */
 ASSIGN i-Lin = i-lin + 1.
 FOR EACH tt-baixa NO-LOCK
    BREAK BY tt-baixa.nr-mistura 
          BY tt-baixa.codigo
          BY tt-baixa.padrao. 

     IF FIRST-OF(tt-baixa.nr-mistura) THEN DO:
        de-tot-realiz = 0.
        FOR EACH b-tt-baixa WHERE
                 b-tt-baixa.nr-mistura = tt-baixa.nr-mistura NO-LOCK.
            ASSIGN de-tot-realiz = de-tot-realiz + b-tt-baixa.qtd.
        END.
     END.

     DDE SEND i-canal SOURCE STRING(tt-baixa.nr-mistura)      ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     DDE SEND i-canal SOURCE STRING(tt-baixa.codigo)          ITEM "L" + TRIM(STRING(i-Lin)) + "C2". 
     DDE SEND i-canal SOURCE STRING(tt-baixa.padrao)          ITEM "L" + TRIM(STRING(i-Lin)) + "C3". 
     DDE SEND i-canal SOURCE STRING(tt-baixa.desc-coloracao)  ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
     DDE SEND i-canal SOURCE STRING(tt-baixa.desc-tipo) + "." ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
     DDE SEND i-canal SOURCE STRING(tt-baixa.prog)            ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
     DDE SEND i-canal SOURCE STRING(tt-baixa.qtd)             ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
     DDE SEND i-canal SOURCE STRING(((tt-baixa.qtd / de-tot-realiz) * 100), "999.99") ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
     DDE SEND i-canal SOURCE STRING(tt-baixa.peso)            ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L'  + TRIM(STRING(i-Lin)) + 'C9")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
     DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".


     /* Horizontal(1=General 2=Left 3=Center 4=Right) Wrap(retorno autom tico), Vertical(1=Top 2=Center 3=Bottom)
      Orientation(0=Horizontal 1=Vertical) */
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C1")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     DDE EXECUTE sys     COMMAND "[alignment(3,true,3,0)]".

     ASSIGN i-Lin = i-Lin + 1.
     ACCUMULATE tt-baixa.prog (TOTAL BY tt-baixa.nr-mistura).
     ACCUMULATE tt-baixa.qtd  (TOTAL BY tt-baixa.nr-mistura).
     ACCUMULATE tt-baixa.peso (TOTAL BY tt-baixa.nr-mistura).

     IF LAST-OF(tt-baixa.nr-mistura)  THEN DO:
        ASSIGN de-ger-prog   = de-ger-prog   + ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.prog
               de-ger-realiz = de-ger-realiz + ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.qtd
               de-ger-peso   = de-ger-peso   + ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.peso.
        DDE SEND i-canal SOURCE "TOTAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.prog)) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.qtd))  ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-baixa.nr-mistura tt-baixa.qtd))  ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C8")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".
        ASSIGN i-lin = i-lin + 1.
     END.
 END.
 DDE SEND i-canal SOURCE "TOTAL GERAL"         ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING(de-ger-prog)   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(de-ger-realiz) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 DDE SEND i-canal SOURCE STRING(de-ger-peso)   ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C8")]'.
 DDE EXECUTE i-canal COMMAND aux-command.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,0)]".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Fardos_Baixados *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-mistura.
   EMPTY TEMP-TABLE tt-baixa.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   ASSIGN br-baixa:TITLE IN FRAME {&FRAME-NAME} = "POSICAO ANALITÖCA DA BAIXA FARDOS DO DIA: " + c-dt-baixa.

   RUN pi-separa-baixas.

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-mistura}
   {&OPEN-QUERY-br-baixa}
   
   APPLY 'value-changed' TO br-mistura IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-mistura IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-baixas C-Win 
PROCEDURE pi-separa-baixas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Gerar Mistura Programada */
/*                          */
FOR EACH mp-fardo WHERE 
         mp-fardo.situacao = 4 AND
         mp-fardo.dt-baixa = DATE(c-dt-baixa) NO-LOCK. 

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(mp-fardo.dt-baixa) + "   " + 
                                        "Fardo: " + TRIM(STRING(mp-fardo.nr-fardo,">>>,>>>,>>9"))).

    FIND mp-mistura WHERE
         mp-mistura.nr-mistura = mp-fardo.nr-mistura NO-LOCK NO-ERROR.

    FIND tt-mistura WHERE
         tt-mistura.nr-mistura = mp-fardo.nr-mistura NO-ERROR.
    IF NOT AVAIL tt-mistura AND AVAIL mp-mistura THEN DO:
       CREATE tt-mistura.
       ASSIGN tt-mistura.nr-mistura   = mp-mistura.nr-mistura
              tt-mistura.dt-mistura   = mp-mistura.dt-mistura
              tt-mistura.tp-mistura   = mp-mistura.tp-mistura
              tt-mistura.qtd-fardos   = mp-mistura.qtd-fardos
              tt-mistura.qtd-bancadas = mp-mistura.qtd-bancadas.
       /* Gera Dados da Mistura Programada */
       FOR EACH mp-distribuicao WHERE
                mp-distribuicao.nr-mistura = mp-fardo.nr-mistura NO-LOCK.
           FIND tt-baixa WHERE
                tt-baixa.nr-mistura = mp-mistura.nr-mistura       AND
                tt-baixa.codigo     = mp-distribuicao.codificacao AND  
                tt-baixa.padrao     = mp-distribuicao.padrao NO-ERROR.
           IF NOT AVAIL tt-baixa THEN DO:
              FIND mp-coloracao  WHERE
                   mp-coloracao.codigo = INT(SUBSTR(mp-distribuicao.codificacao,2,1)) NO-LOCK NO-ERROR.
              FIND mp-tipo  WHERE
                   mp-tipo.codigo = INT(SUBSTR(mp-distribuicao.codificacao,3,1)) NO-LOCK NO-ERROR.
              CREATE tt-baixa.
              ASSIGN tt-baixa.nr-mistura     = mp-mistura.nr-mistura
                     tt-baixa.codigo         = mp-distribuicao.codificacao
                     tt-baixa.desc-coloracao = IF AVAIL mp-coloracao 
                                                   THEN mp-coloracao.tonalidade ELSE ''
                     tt-baixa.desc-tipo      = IF AVAIL mp-tipo
                                                   THEN mp-tipo.tipo ELSE '' 
                     tt-baixa.padrao         = mp-distribuicao.padrao.
           END.
           ASSIGN tt-baixa.prog = tt-baixa.prog + mp-mistura.qtd-bancadas.
       END.
    END.
END.
/* Gerar Mistura Realizada */
/*                         */
FOR EACH mp-fardo WHERE 
         mp-fardo.situacao = 4 AND
         mp-fardo.dt-baixa = DATE(c-dt-baixa) NO-LOCK. 

    RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(mp-fardo.dt-baixa) + "   " + 
                                        "Fardo: " + TRIM(STRING(mp-fardo.nr-fardo,">>>,>>>,>>9"))).

    FIND mp-coloracao  WHERE
         mp-coloracao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
    
    FIND mp-tipo  WHERE
         mp-tipo.codigo = mp-fardo.cd-tipo NO-LOCK NO-ERROR.

    FIND tt-baixa WHERE
         tt-baixa.nr-mistura = mp-fardo.nr-mistura AND 
         tt-baixa.codigo = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                            STRING(mp-fardo.cd-tipo)  AND
         tt-baixa.padrao = mp-fardo.padrao NO-ERROR.
    IF NOT AVAIL tt-baixa THEN DO:
       CREATE tt-baixa.
       ASSIGN tt-baixa.nr-mistura = mp-fardo.nr-mistura
              tt-baixa.codigo     = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                                     STRING(mp-fardo.cd-tipo)   
              tt-baixa.desc-coloracao = IF AVAIL mp-coloracao 
                                           THEN mp-coloracao.tonalidade ELSE ''
              tt-baixa.desc-tipo      = IF AVAIL mp-tipo
                                           THEN mp-tipo.tipo ELSE '' 
              tt-baixa.padrao = mp-fardo.padrao.         
    END.
    ASSIGN tt-baixa.qtd  = tt-baixa.qtd  + 1
           tt-baixa.peso = tt-baixa.peso + mp-fardo.peso.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais C-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN fi-tot-prog   = 0
        fi-tot-realiz = 0
        fi-tot-peso   = 0. 
 FOR EACH tt-baixa WHERE 
          tt-baixa.nr-mistura = tt-mistura.nr-mistura NO-LOCK.
     ASSIGN fi-tot-prog   = fi-tot-prog   + tt-baixa.prog
            fi-tot-realiz = fi-tot-realiz + tt-baixa.qtd
            fi-tot-peso   = fi-tot-peso   + tt-baixa.peso.
 END.

 DISP fi-tot-prog 
      fi-tot-realiz 
      fi-tot-peso 
      WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

