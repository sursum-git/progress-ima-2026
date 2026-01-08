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

{include/tt-edit.i}
{include/pi-edit.i}

/* ***************************  Definitions  ************************** */
DEFINE buffer empresa for mgcad.empresa.

/* Parameters Definitions ---                                           */
DEF OUTPUT PARAMETER c-texto-msg LIKE mensagem.texto-mensag.
DEF OUTPUT PARAMETER c-doca-msg LIKE ob-etiqueta.localizacao.

DEF BUFFER b-mensagem FOR mensagem.                                                         

/* Local Variable Definitions ---                                       */
DEF VAR c-empresa   AS CHAR.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".

/* Variaveis da Rotina de ImpressÆo */
DEF VAR l-ok         AS LOG.
DEF VAR i-saida      AS INT.
DEF VAR i-pag        AS INT.
DEF VAR i-lin        AS INT.
DEF VAR i-ct         AS INT.
DEF VAR c-saida      AS CHAR.
DEF VAR i-num-copias AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-msg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mensagem mensagem-ext

/* Definitions for BROWSE br-msg                                        */
&Scoped-define FIELDS-IN-QUERY-br-msg mensagem.cod-mensagem ~
mensagem.descricao 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-msg 
&Scoped-define QUERY-STRING-br-msg FOR EACH mgcad.mensagem NO-LOCK, ~
      EACH mensagem-ext OF mgcad.mensagem ~
      WHERE mensagem-ext.utilizacao = 3 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-msg OPEN QUERY br-msg FOR EACH mgcad.mensagem NO-LOCK, ~
      EACH mensagem-ext OF mgcad.mensagem ~
      WHERE mensagem-ext.utilizacao = 3 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-msg mensagem mensagem-ext
&Scoped-define FIRST-TABLE-IN-QUERY-br-msg mensagem
&Scoped-define SECOND-TABLE-IN-QUERY-br-msg mensagem-ext


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-msg}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 rt-buttom br-msg edt-msg bt-ok ~
bt-cancela ebt-excel bt-imp-devol bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS edt-msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 ebt-excel 

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

DEFINE BUTTON bt-imp-devol 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 8 BY 1 TOOLTIP "Imprimir Relat¢rio das Mensagens de Devolu‡Æo.".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON ebt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1 TOOLTIP "Gerar Planilha Excel".

DEFINE VARIABLE edt-msg AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 39.14 BY 9.63 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 11.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 39 BY .75
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 76 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-msg FOR 
      mensagem, 
      mensagem-ext SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-msg D-Dialog _STRUCTURED
  QUERY br-msg NO-LOCK DISPLAY
      mensagem.cod-mensagem FORMAT ">>>9":U WIDTH 6.14 LABEL-FGCOLOR 12 LABEL-BGCOLOR 8 LABEL-FONT 6
      mensagem.descricao WIDTH 23.86 LABEL-FGCOLOR 12 LABEL-BGCOLOR 8 LABEL-FONT 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33.72 BY 10.5
         FONT 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-msg AT ROW 1.33 COL 3.29
     edt-msg AT ROW 2.25 COL 37.72 NO-LABEL
     bt-ok AT ROW 12.54 COL 3
     bt-cancela AT ROW 12.54 COL 14
     ebt-excel AT ROW 12.54 COL 38 WIDGET-ID 10
     bt-imp-devol AT ROW 12.54 COL 53 WIDGET-ID 72
     bt-ajuda AT ROW 12.54 COL 66.14
     "Texto da Mensagem" VIEW-AS TEXT
          SIZE 18 BY .54 AT ROW 1.46 COL 38.72
          BGCOLOR 8 FGCOLOR 12 FONT 6
     RECT-1 AT ROW 1 COL 2
     RECT-2 AT ROW 1.38 COL 37.72
     rt-buttom AT ROW 12.29 COL 2
     SPACE(0.56) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Seleciona Mensagens de DEVOLU€ÇO"
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
/* BROWSE-TAB br-msg rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON ebt-excel IN FRAME D-Dialog
   6                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-msg
/* Query rebuild information for BROWSE br-msg
     _TblList          = "mgcad.mensagem,espec.mensagem-ext OF mgcad.mensagem"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[2]         = "espec.mensagem-ext.utilizacao = 3"
     _FldNameList[1]   > mgcad.mensagem.cod-mensagem
"mensagem.cod-mensagem" ? ">>>9" "integer" ? ? ? 8 12 6 no ? no no "6.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > mgcad.mensagem.descricao
"mensagem.descricao" ? ? "character" ? ? ? 8 12 6 no ? no no "23.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-msg */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Seleciona Mensagens de DEVOLU€ÇO */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-msg
&Scoped-define SELF-NAME br-msg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-msg D-Dialog
ON VALUE-CHANGED OF br-msg IN FRAME D-Dialog
DO:
   ASSIGN edt-msg:SCREEN-VALUE = TRIM(mensagem.descricao) + " - " + mensagem.texto-mensag.
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


&Scoped-define SELF-NAME bt-imp-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imp-devol D-Dialog
ON CHOOSE OF bt-imp-devol IN FRAME D-Dialog
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   ASSIGN c-texto-msg = edt-msg:SCREEN-VALUE
          c-doca-msg = IF AVAIL mensagem 
                       THEN SUBSTR(mensagem.char-2,100,6)
                       ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ebt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ebt-excel D-Dialog
ON CHOOSE OF ebt-excel IN FRAME D-Dialog /* Button 2 */
DO:
   ASSIGN arq-saida = SESSION:TEMP-DIRECTORY + "Mensagens Devolu‡Æo.XLS".
   RUN esdlg/d01essp0204.w (INPUT-OUTPUT arq-saida).
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel.
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess -lo,  abra-o atrav‚s do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK. 
   END.
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
  DISPLAY edt-msg 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 RECT-2 rt-buttom br-msg edt-msg bt-ok bt-cancela ebt-excel 
         bt-imp-devol bt-ajuda 
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  APPLY 'VALUE-CHANGED' TO br-msg IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel D-Dialog 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
CREATE "Excel.Application" chExcelApp NO-ERROR.
IF chExcelApp <> ? THEN /* Cria a Planilha */
   ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar  Visivel */
          chExcelApp:SheetsInNewWorkbook = 1 /* Nõ PLANILHAS A SEREM CRIADAS */
          chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
          chworksheet            = chExcelapp:sheets:ITEM(1)
          chworksheet:PageSetup:PrintGridlines = TRUE              /* Imprimir Linhas de Grade */
          chworksheet:PageSetup:CenterHorizontally = TRUE          /* Centraliza Linhas Horizontais */
          chworksheet:PageSetup:CenterVertically   = FALSE         /* Centraliza Linhas Verticais */
          chworksheet:PageSetup:rightheader = "&d - &t" + "  Pagina: &9&P De &N" 
          chworksheet:pagesetup:ORIENTATION = 2                    /* PAISAGEM */
          chworksheet:pagesetup:printTitleRows = "$1:$2"           /* Imprimir Sempre em cada Pagina as linhas 1 a 3*/
          chworksheet:pagesetup:zoom = 85.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel D-Dialog 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pi-abre-excel.
  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL nÆo foi encontrado. NÆo ‚ possivel a execu‡Æo do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  RUN pi-monta-planilha.

  /* Posiciona o Foco no Inicio da Planilha */
  chExcelApp:Range("A1"):SELECT.
  chExcelApp:Range("A:A"):EntireColumn:AutoFit.

  /* Posiciona na Planilha 1, Salva e Fecha */
  chWorkSheet = chExcelapp:Sheets:ITEM(1).
  chWorkbook:Worksheets(1):activate.

  /* Salva e Fecha Planilha */
  IF chExcelApp:Version < "12":U THEN DO.
     ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xls".
     OS-DELETE VALUE(arq-saida).
     chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
  END.
  ELSE DO:
     ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xlsx".
     OS-DELETE VALUE(arq-saida).
     chWorkBook:SaveAs(arq-saida,,,,,,,).  /* Salva na VersÆo da planilha da ESTA€ÇO */
  END.

  chWorkBook:CLOSE().
  chExcelApp:QUIT().
  RELEASE OBJECT chExcelApp. 
  RELEASE OBJECT chworkBook.
  RELEASE OBJECT chworksheet.






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
     "DATA: "                                  AT  51
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  57
     "HORA: "                                  AT  84
     STRING(TIME,"hh:mm:ss")                   AT  90
     "PAG:"                                    AT 128
     i-pag FORMAT ">>>"                        AT 133
     SKIP(1).

 PUT "RELATORIO DE MENSAGENS DA DEVOLU€ÇO DE CLIENTES" FORMAT "X(50)" AT 40 SKIP(1).

 PUT "COD. MENSAGEM             TEXTO DA MENSAGEM " AT 1.
 PUT "---- -------------------- -------------------------------------------------------------------------------------------------------------" AT 1. 
 ASSIGN i-pag = i-pag + 1.


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
  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).
  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          PUT CONTROL "~033&l1O~033(s16H". /* ORIENTA€ÇO PAISAGEM & COMPACTA */ 
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0204b.txt".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag        =  1
            i-lin        = 99.
     FOR EACH b-mensagem where
              b-mensagem.log-1 = TRUE NO-LOCK.

         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.

         PUT b-mensagem.cod-mensagem  FORMAT ">>>9"   AT 1
             b-mensagem.descricao     FORMAT "x(20)"  AT 6.
         RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(b-mensagem.texto-mensag),CHR(13)," "),CHR(10)," "), INPUT 108). 
         FOR EACH tt-editor:
             PUT tt-editor.conteudo AT 27 SKIP.
             ASSIGN i-lin = i-lin + 1.
         END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha D-Dialog 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Nomear Aba da Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkSheet:NAME = "Mensagem".
 chWorkSheet:TAB:ColorIndex = 19.

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkbook:Worksheets(1):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "CADASTRO DE MENSAGEM DA DEVOLU€ÇO DE CLIENTES".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:C1"):SELECT().
 ChWorksheet:range("B1:C1"):Merge.
 Chworksheet:Range("B1:C1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:C1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:C1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:C1"):Interior:ColorIndex = 2. /* Branco */

 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 40
        chWorkSheet:Rows("1:1"):FONT:SIZE = 18
        chWorkSheet:Rows("1:1"):FONT:bold = FALSE.

 /* Inserir Logotipo da Tear e Alinhar Via Tamanho e Altura Logotipo */
 ChWorkSheet:range("A1"):SELECT().
 ChWorkSheet:Pictures:INSERT("image\logo-excel.bmp"):SELECT. 
 chExcelApp:SELECTION:ShapeRange:ScaleWidth(1.9,FALSE,FALSE).
 chExcelApp:SELECTION:ShapeRange:ScaleHeight(0.61,FALSE,FALSE). 
 chExcelApp:SELECTION:ShapeRange:IncrementLeft(-49.5).
 chExcelApp:SELECTION:ShapeRange:IncrementTop( -48.75).

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A2"):VALUE = "CODIGO"
        chworksheet:range("B2"):VALUE = "DESCRI€ÇO"
        chworksheet:range("C2"):VALUE = "TEXTO DA MENSAGEM".

 /* Ajustar o Tamanho Dentro da Celula */     
 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE.

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:COLUMNS("A"):ColumnWidth =  16
        chWorkSheet:COLUMNS("B"):ColumnWidth =  20
        chWorkSheet:COLUMNS("C"):ColumnWidth = 108.

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:C"):NumberFormat        = "@".

 /* Configura Cabe‡alho das Colunas */
 chWorkSheet:Range("A2:C2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 ASSIGN i-Lin    = 3.
 FOR EACH b-mensagem where
          b-mensagem.log-1 = TRUE NO-LOCK.

      ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = STRING(b-mensagem.cod-mensagem, ">>>9") 
             chworksheet:range("B" + STRING(i-lin)):VALUE = b-mensagem.descricao.
      RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(b-mensagem.texto-mensag),CHR(13)," "),CHR(10)," "), INPUT 108). 
      FOR EACH tt-editor:
          chworksheet:range("C" + STRING(i-lin)):VALUE = tt-editor.conteudo.
          ASSIGN i-lin = i-lin + 1.
      END.
 END.
 ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$C$" + STRING(i-lin).  /* Marcar Area de Impressao */



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
  {src/adm/template/snd-list.i "mensagem"}
  {src/adm/template/snd-list.i "mensagem-ext"}

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

