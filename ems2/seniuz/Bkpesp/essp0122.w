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
{include/i-prgvrs.i ESSP0169 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */

/* Temp Table Definitions ---             */

DEFINE TEMP-TABLE tt-estoque 
       FIELD codigo         AS CHAR 
       FIELD Padrao         AS CHAR
       FIELD cor            AS INT
       FIELD tipo           AS INT
       FIELD letra          AS CHAR 
       FIELD desc-coloracao AS CHAR 
       FIELD desc-tipo      AS CHAR
       FIELD tamanho        AS CHAR
       FIELD qtd            AS INT 
       FIELD peso           AS DEC
       INDEX indice1 codigo padrao.


/* Local Variable Definitions ---                                       */
DEF VAR h-acomp            AS HANDLE NO-UNDO.
DEF VAR i-lin              AS INT.
DEF VAR i-pag              AS INT.
DEF VAR c-letra            AS CHAR FORMAT "x(1)".
DEF VAR de-tot-peso-letra  AS DEC.
DEF VAR de-tot-qtd-letra   AS DEC.


/* Variaveis da Rotina de ImpressÆo */
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* Variavies de Parƒmetros */
DEF VAR c-padrao-ini   AS CHAR INIT "".
DEF VAR c-padrao-fin   AS CHAR INIT "ZZZZZZZZZZZZZZZZZZZZ".
DEF VAR i-cor-ini      AS INT.
DEF VAR i-cor-fin      AS INT  INIT 99.
DEF VAR i-tipo-ini     AS INT.
DEF VAR i-tipo-fin     AS INT  INIT 99.
DEF VAR i-tamanho-ini  AS INT. 
DEF VAR i-tamanho-fin  AS INT  INIT 99.
DEF VAR c-dt-receb-ini AS CHAR INIT "01/01/0001".
DEF VAR c-dt-receb-fin AS CHAR INIT "31/12/9999".
DEF VAR c-deposito-ini AS CHAR INIT "".
DEF VAR c-deposito-fin AS CHAR INIT "ZZZZZZZZ".
DEF VAR c-local-ini    AS CHAR INIT "".
DEF VAR c-local-fin    AS CHAR INIT "ZZZZZZZZ".
DEF VAR c-letra-ini    AS CHAR INIT "A00".
DEF VAR c-letra-fin    AS CHAR INIT "Z99".
DEF VAR l-ok           AS LOG.
DEF VAR c-empresa      AS CHAR.

/* Variaveis Usadas Na Gera‡Æo da Planilha Excel */
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
&Scoped-define BROWSE-NAME br-estoque

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-estoque

/* Definitions for BROWSE br-estoque                                    */
&Scoped-define FIELDS-IN-QUERY-br-estoque tt-estoque.codigo tt-estoque.padrao tt-estoque.desc-coloracao tt-estoque.desc-tipo tt-estoque.tamanho tt-estoque.qtd tt-estoque.peso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-estoque   
&Scoped-define SELF-NAME br-estoque
&Scoped-define OPEN-QUERY-br-estoque RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-estoque NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-estoque tt-estoque
&Scoped-define FIRST-TABLE-IN-QUERY-br-estoque tt-estoque


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-estoque}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-estoque bt-param bt-imprime ~
bt-excel bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-fardo fi-tot-peso 

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

DEFINE VARIABLE fi-tot-fardo AS INTEGER FORMAT "ZZZ,ZZZ,ZZ9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.57 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-peso AS DECIMAL FORMAT "      ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 21
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-estoque FOR 
      tt-estoque SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-estoque C-Win _FREEFORM
  QUERY br-estoque NO-LOCK DISPLAY
      tt-estoque.codigo         COLUMN-LABEL "Codigo"      WIDTH 6
      tt-estoque.padrao         COLUMN-LABEL "Padrao"     FORMAT "x(12)" WIDTH 12.00
      tt-estoque.desc-coloracao COLUMN-LABEL "Tonalidade" FORMAT "x(15)" WIDTH 12.00
      tt-estoque.desc-tipo      COLUMN-LABEL "Tipo"       FORMAT "x(10)" WIDTH 8.00
      tt-estoque.tamanho        COLUMN-LABEL "Tamanho"    FORMAT "x(20)" WIDTH 12.00
      tt-estoque.qtd            COLUMN-LABEL "Quantidade"
      tt-estoque.peso           COLUMN-LABEL "Peso Fardos" WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 78 BY 19.5
         FONT 1
         TITLE "Posi‡Æo do Estoque Fardos AlgodÆo" ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-estoque AT ROW 1.25 COL 1
     bt-param AT ROW 1.29 COL 81
     bt-imprime AT ROW 2.75 COL 81
     bt-excel AT ROW 4.25 COL 81
     bt-exit AT ROW 18.96 COL 81
     bt-ajuda AT ROW 20.29 COL 81
     fi-tot-fardo AT ROW 20.96 COL 52.43 COLON-ALIGNED NO-LABEL
     fi-tot-peso AT ROW 20.96 COL 60.43 COLON-ALIGNED NO-LABEL
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
         TITLE              = "Estoque Fardos de AlgodÆo"
         COLUMN             = 18.72
         ROW                = 7.71
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
/* BROWSE-TAB br-estoque rt-buttom DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-tot-fardo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-peso IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-estoque
/* Query rebuild information for BROWSE br-estoque
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-estoque NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-estoque */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Estoque Fardos de AlgodÆo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Estoque Fardos de AlgodÆo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-estoque
&Scoped-define SELF-NAME br-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estoque C-Win
ON ROW-DISPLAY OF br-estoque IN FRAME DEFAULT-FRAME /* Posi‡Æo do Estoque Fardos AlgodÆo */
DO:
   /*
   IF tt-baixa.prog >  0 THEN
      RUN pi-cor (INPUT 12). */
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
    RUN esdlg/d01essp0122.w (OUTPUT arq-saida).
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
   RUN esp/essp0122a.w (INPUT-OUTPUT c-padrao-ini,  
                        INPUT-OUTPUT c-padrao-fin,  
                        INPUT-OUTPUT i-cor-ini,
                        INPUT-OUTPUT i-cor-fin,
                        INPUT-OUTPUT i-tipo-ini,
                        INPUT-OUTPUT i-tipo-fin,
                        INPUT-OUTPUT i-tamanho-ini,
                        INPUT-OUTPUT i-tamanho-fin, 
                        INPUT-OUTPUT c-dt-receb-ini,
                        INPUT-OUTPUT c-dt-receb-fin,
                        INPUT-OUTPUT c-deposito-ini,
                        INPUT-OUTPUT c-deposito-fin,
                        INPUT-OUTPUT c-local-ini,   
                        INPUT-OUTPUT c-local-fin,   
                        INPUT-OUTPUT c-letra-ini,   
                        INPUT-OUTPUT c-letra-fin,   
                        INPUT-OUTPUT l-ok).                  

   IF l-ok THEN                                     
      RUN pi-popula-browse.
   ASSIGN c-win:SENSITIVE = YES.

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

  /* ASSIGN c-dt-baixa = STRING(TODAY). */

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
  DISPLAY fi-tot-fardo fi-tot-peso 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom br-estoque bt-param bt-imprime bt-excel bt-exit bt-ajuda 
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

 tt-estoque.codigo:FGCOLOR IN BROWSE {&browse-name}         = p-cor.
 tt-estoque.padrao:FGCOLOR IN BROWSE {&browse-name}         = p-cor.
 tt-estoque.desc-coloracao:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-estoque.desc-tipo:FGCOLOR IN BROWSE {&browse-name}      = p-cor.
 tt-estoque.qtd:FGCOLOR IN BROWSE {&browse-name}            = 1.
 tt-estoque.peso:FGCOLOR IN BROWSE {&browse-name}           = 1.
    
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
        "DATA: "                                  AT  45
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  51
        "HORA: "                                  AT  67
        STRING(TIME,"hh:mm:ss")                   AT  73
        "PAG:"                                    AT  93
        i-pag FORMAT ">>>"                        AT  98
        SKIP(1).

    PUT "RELATORIO DA POSICAO DE ESTOQUE DE FARDOS DO DIA" AT 19
        STRING(TODAY, "99/99/9999") FORMAT "x(10)" AT 68 SKIP(1).

    PUT "CODIGO PADRAO               TONALIDADE      TIPO         TAMANHO           QUANTIDADE PESO DOS FARDO" AT 1. 
    PUT "------ -------------------- --------------- ------------ ----------------- ---------- --------------" AT 1. 
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
/*         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61. */
         /* Altera‡Æo efetudada, devido nÆo IMPRESSÇO */
         /* do relatorio via REMOTO */
         OUTPUT TO PRINTER PAGED PAGE-SIZE 61.
         PUT CONTROL "~033E~033(s14H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0122.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-lin             = 99
           i-pag             =  1
           de-tot-peso-letra =  0
           de-tot-qtd-letra  =  0
           c-letra           = "".

    FOR EACH tt-estoque  NO-LOCK
          BY tt-estoque.cor
          BY tt-estoque.tipo
          BY tt-estoque.letra.

        IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        IF c-letra <> "" AND c-letra <> tt-estoque.letra THEN DO:
           PUT "TOTAL DA LETRA.......:" AT 53.
           PUT de-tot-qtd-letra  FORMAT ">,>>>,>>>"      AT 77. 
           PUT de-tot-peso-letra FORMAT ">>>,>>>,>>9.99" AT 87 SKIP(1). 
           ASSIGN de-tot-peso-letra = 0
                  de-tot-qtd-letra  = 0
                  i-lin             = i-lin + 2.
        END.
        ASSIGN c-letra = tt-estoque.letra.
        PUT tt-estoque.codigo         FORMAT "x(3)"           AT  1
            tt-estoque.padrao         FORMAT "x(20)"          AT  8
            tt-estoque.desc-coloracao FORMAT "x(15)"          AT 29
            tt-estoque.desc-tipo      FORMAT "X(12)"          AT 45
            tt-estoque.tamanho        FORMAT "x(17)"          AT 58
            tt-estoque.qtd            FORMAT ">,>>>,>>>"      AT 77
            tt-estoque.peso           FORMAT ">>>,>>>,>>9.99" AT 87.

        ACCUMULATE tt-estoque.qtd  (TOTAL).
        ACCUMULATE tt-estoque.peso (TOTAL).
        ASSIGN de-tot-peso-letra = de-tot-peso-letra + tt-estoque.peso
               de-tot-qtd-letra  = de-tot-qtd-letra  + tt-estoque.qtd
               i-lin             = i-lin + 1.

    END.

    IF c-letra <> "" AND de-tot-qtd-letra <> 0 THEN DO:
       PUT "TOTAL DA LETRA.......:" AT 53.
       PUT de-tot-qtd-letra  FORMAT ">,>>>,>>>"      AT 77. 
       PUT de-tot-peso-letra FORMAT ">>>,>>>,>>9.99" AT 87 SKIP(1). 
       ASSIGN de-tot-peso-letra = 0
              de-tot-qtd-letra  = 0
              i-lin             = i-lin + 2.
    END.

    IF (ACCUM TOTAL tt-estoque.peso) <> 0 THEN DO:
       PUT "---------- --------------" AT 76 SKIP.
       PUT "TOTAL GERAL..........:" AT 53.
       PUT (ACCUM TOTAL tt-estoque.qtd)  FORMAT ">,>>>,>>>"      AT 77. 
       PUT (ACCUM TOTAL tt-estoque.peso) FORMAT ">>>,>>>,>>9.99" AT 87 SKIP(1). 
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


 /* Cabe‡alho  da Planilha */
 ASSIGN c-Lin = c-empresa + "             " + " POSI€ÇO DE ESTOQUE DE FARDOS DO DIA: "  + STRING(TODAY, "99/99/9999"). 
 DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
 DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C8")]'.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

 /* Cabe‡alho dos Dados */
 ASSIGN i-lin = 3.
 DDE SEND i-canal SOURCE "CODIGO"     ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
 DDE SEND i-canal SOURCE "PADRAO"     ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
 DDE SEND i-canal SOURCE "TONALIDADE" ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
 DDE SEND i-canal SOURCE "TIPO"       ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE "TAMANHO"    ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 DDE SEND i-canal SOURCE "QUANTIDADE" ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE "PESO FARDO" ITEM "L" + TRIM(STRING(i-Lin)) + "C7".

 DDE EXECUTE i-canal COMMAND '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C7")]'.
 /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
 DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(8.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(20.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(25.00)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".

 DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(15.00)]".
 
 
 DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

 DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
 DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
 DDE EXECUTE sys     COMMAND "[format.number(~"###.##0,00~")]".
 DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]". 

  /* Montagem das Celulas de Dados */
 ASSIGN i-Lin = i-lin + 1.
 FOR EACH tt-estoque NO-LOCK
       BY tt-estoque.cor 
       BY tt-estoque.tipo
       BY tt-estoque.letra. 

     DDE SEND i-canal SOURCE STRING(tt-estoque.codigo)          ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
     DDE SEND i-canal SOURCE STRING(tt-estoque.padrao)          ITEM "L" + TRIM(STRING(i-Lin)) + "C2". 
     DDE SEND i-canal SOURCE STRING(tt-estoque.desc-coloracao)  ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
     DDE SEND i-canal SOURCE STRING(tt-estoque.desc-tipo) + "." ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
     DDE SEND i-canal SOURCE STRING(tt-estoque.tamanho)         ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
     DDE SEND i-canal SOURCE STRING(tt-estoque.qtd)             ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
     DDE SEND i-canal SOURCE STRING(tt-estoque.peso)            ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L'  + TRIM(STRING(i-Lin)) + 'C7")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
     DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".


     /* Horizontal(1=General 2=Left 3=Center 4=Right) Wrap(retorno autom tico), Vertical(1=Top 2=Center 3=Bottom)
      Orientation(0=Horizontal 1=Vertical) */
     ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C1")]'.
     DDE EXECUTE i-canal COMMAND aux-command.
     DDE EXECUTE sys     COMMAND "[alignment(3,true,3,0)]".

     ASSIGN i-Lin = i-Lin + 1.
     ACCUMULATE tt-estoque.qtd  (TOTAL).
     ACCUMULATE tt-estoque.peso (TOTAL).

 END.
 DDE SEND i-canal SOURCE "TOTAL GERAL"         ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
 DDE SEND i-canal SOURCE STRING(ACCUM TOTAL tt-estoque.qtd)   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
 DDE SEND i-canal SOURCE STRING(ACCUM TOTAL tt-estoque.peso)   ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
 ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C7")]'.
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
   {utp/ut-liter.i Selecionando_Fardos_Em_Estoque *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-estoque.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   ASSIGN br-estoque:TITLE IN FRAME {&FRAME-NAME} = "POSICAO DO ESTOQUE FARDOS ALGODÇO: " + STRING(TODAY, "99/99/9999").

   RUN pi-separa-estoque.

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-estoque}
   
   APPLY 'value-changed' TO br-estoque IN FRAME {&FRAME-NAME}.
   APPLY 'entry' TO br-estoque IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-estoque C-Win 
PROCEDURE pi-separa-estoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH mp-fardo WHERE 
         mp-fardo.situacao      = 3  AND /* Fardos em Estoque */
         mp-fardo.padrao       >= c-padrao-ini   AND      
         mp-fardo.padrao       <= c-padrao-fin   AND    
         mp-fardo.cd-coloracao >= i-cor-ini      AND      
         mp-fardo.cd-coloracao <= i-cor-fin      AND    
         mp-fardo.cd-tipo      >= i-tipo-ini     AND      
         mp-fardo.cd-tipo      <= i-tipo-fin     AND    
         mp-fardo.cd-compr     >= i-tamanho-ini  AND      
         mp-fardo.cd-compr     <= i-tamanho-fin  AND    
         mp-fardo.cod-depos    >= c-deposito-ini AND 
         mp-fardo.cod-depos    <= c-deposito-fin AND
         mp-fardo.cod-localiz  >= c-local-ini    AND
         mp-fardo.cod-localiz  <= c-local-fin  
         NO-LOCK,
    EACH mp-entr-mat WHERE 
         mp-entr-mat.dt-recebimento >= DATE(c-dt-receb-ini) AND 
         mp-entr-mat.dt-recebimento <= DATE(c-dt-receb-fin) AND  
         mp-entr-mat.nr-cdr          = mp-fardo.nr-cdr NO-LOCK.  

    RUN pi-acompanhar IN h-acomp (INPUT "Fardo: " + TRIM(STRING(mp-fardo.nr-fardo,"9999,9999"))).

    IF mp-fardo.letra + STRING(mp-fardo.cd-coloracao) +  STRING(mp-fardo.cd-tipo) < c-letra-ini THEN
       NEXT.

    IF mp-fardo.letra + STRING(mp-fardo.cd-coloracao) +  STRING(mp-fardo.cd-tipo) > c-letra-fin THEN
        NEXT.

    FIND mp-coloracao  WHERE
         mp-coloracao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
    
    FIND mp-tipo  WHERE
         mp-tipo.codigo = mp-fardo.cd-tipo NO-LOCK NO-ERROR.

    FIND mp-classificacao WHERE
         mp-classificacao.codigo = mp-fardo.cd-compr NO-LOCK NO-ERROR.

    FIND tt-estoque WHERE
         tt-estoque.codigo = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                              STRING(mp-fardo.cd-tipo)  AND
         tt-estoque.padrao = mp-fardo.padrao NO-ERROR.
    IF NOT AVAIL tt-estoque THEN DO:
       CREATE tt-estoque.
       ASSIGN tt-estoque.codigo         = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                                           STRING(mp-fardo.cd-tipo)   
              tt-estoque.desc-coloracao = IF AVAIL mp-coloracao 
                                           THEN mp-coloracao.tonalidade ELSE ''
              tt-estoque.desc-tipo      = IF AVAIL mp-tipo
                                           THEN mp-tipo.tipo ELSE '' 
              tt-estoque.padrao         = mp-fardo.padrao
              tt-estoque.cor            = mp-fardo.cd-coloracao
              tt-estoque.tipo           = mp-fardo.cd-tipo
              tt-estoque.letra          = mp-fardo.letra 
              tt-estoque.tamanho        = STRING(mp-classificacao.compr-min,">>9.99") + "  A  " +
                                          STRING(mp-classificacao.compr-max,">>9.99").

    END.
    ASSIGN tt-estoque.qtd  = tt-estoque.qtd  + 1
           tt-estoque.peso = tt-estoque.peso + mp-fardo.peso.
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

 ASSIGN fi-tot-fardo  = 0
        fi-tot-peso   = 0. 
 FOR EACH tt-estoque NO-LOCK.
     ASSIGN fi-tot-fardo  = fi-tot-fardo  + tt-estoque.qtd
            fi-tot-peso   = fi-tot-peso   + tt-estoque.peso.
 END.

 DISP fi-tot-fardo 
      fi-tot-peso 
      WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

