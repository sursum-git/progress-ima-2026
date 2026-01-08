DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF var chWorksheet AS COM-HANDLE NO-UNDO.
DEF VAR cFileName   AS CHAR.
DEF VAR i-lin       AS INT INITIAL 2.
DEF VAR i-ct        AS INT.


CREATE "Excel.Application" chExcelApp.

/* Cria a Planilha */
ASSIGN chExcelApp:VISIBLE     = FALSE  /* A Planilha NÇO Ficar  Visivel */
       chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
       chworksheet            = chExcelapp:sheets:ITEM(1).

/* Incluir Uma Nova Planilha */
ChWorksheet = chWorkbook:Worksheets(1).
chWorkbook:Worksheets:add(,chWorksheet).

    ASSIGN cFileName = "C:\TEMP\TESTE.XLS".

/* Salva e Fecha a Planilha */
chExcelApp:DisplayAlerts = FALSE. 
chWorkBook:Save().
chWorkBook:SaveAs(cFileName,,,,,,,).
chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.

/* Open Documento */
ASSIGN chExcelApp:VISIBLE     = FALSE 
       chWorkbook  = chExcelApp:Workbooks:OPEN(cFileName).

/* Incluir Uma Nova Planilha */
ChWorksheet = chWorkbook:Worksheets(1).
chWorkbook:Worksheets:add(,chWorksheet).


/* Seleciona a Workbook */
chWorkbook  = chExcelApp:Workbooks(1).

/* Select a Worksheet */
chWorkbook:Worksheets(1):Activate.
ChWorksheet = chWorkbook:Worksheets(1).


/* Inclui Valores */
ASSIGN chWorksheet:range("B1"):VALUE = "Valor"
       chWorksheet:range("B2"):VALUE = 255
       chWorksheet:range("B3"):VALUE = 100
       chWorksheet:range("B4"):VALUE = 250
       chWorksheet:range("B5"):VALUE = 400
       chWorksheet:range("B6"):VALUE = 100
       chWorksheet:range("B7"):VALUE = 600.

/* Altera formato da Celula */
ASSIGN chWorksheet:Columns("A:A"):FONT:ColorIndex = 2
       chWorksheet:Columns("A:A"):FONT:NAME       = "Courrier New"
       chWorksheet:Columns("A:A"):FONT:Bold       = TRUE
       chWorksheet:Columns("A:A"):FONT:Italic     = TRUE.

/* Configura Estilo de Linha */
ASSIGN chWorksheet:Columns("A:A"):FONT:UNDERLINE  = 2.

/* Inclui uma Formula */
ASSIGN chWorksheet:range("A8"):VALUE   = "Total"
       chWorksheet:range("B8"):Formula = "=SUM(B2:B7)".


/* Alterar Formato da Celula Para Numero */
ASSIGN chWorksheet:range("B8"):NumberFormat = 0. /* @ Caracter */

/* Configura Alinhamento Horizontal 
   Alinhamento a Direira  = -4152
   Alinhamento a Esquerda = -4131 */

/* Configura Alinhamento Horizontal */
ASSIGN chWorksheet:range("B:B"):HorizontalAlignment = -4152.

chWorkSheet:Range("B:B"):FONT:SIZE     = 16.
chWorkSheet:COLUMNS("B:B"):ColumnWidth = 20.
chworksheet:range("B:B"):NumberFormat  = "###.###.##0,00".




/* Congela Celula */
chWorksheet:Range("A2"):SELECT.
chExcelApp:ActiveWindow:freezePanes = TRUE.

/* Altera Cor da Celula */
chWorksheet:Columns("A:A"):Interior:ColorIndex = 5.


/* Salva e Fecha a Planilha */
chExcelApp:DisplayAlerts = FALSE. 
chWorkBook:Save().

/*chWorkBook:SaveAs(cFileName,,,,,,,).*/
if chExcelapp:Version begins "8":U THEN
    chWorkBook:SaveAs(cFileName,39,,,,,,,).
ELSE 
    chWorkBook:SaveAs(cFileName,,,,,,,).

chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.
















































