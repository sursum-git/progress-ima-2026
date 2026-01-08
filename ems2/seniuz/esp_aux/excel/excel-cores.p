DEF VAR chExcelApp       AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook       AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet      AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheetRange AS COM-HANDLE. 
DEF VAR chChart          AS COM-HANDLE. 
DEF VAR cFileName   AS CHAR.
DEF VAR c-col       AS CHAR.
DEF VAR c-work      AS CHAR.
DEF VAR i-lin       AS INT.
DEF VAR i-col       AS INT.


ASSIGN c-col = "A;B;C;D;E;F;E;G;H;I;J;L;M;N;O;P;Q;R;S;T;U;V;W;X;Y;Z;AA;AB;AC;AD;AE;AF;AG;AH;AI;AJ;AL;AM;AN;AO;AP;AQ;AR;AS;AT;AU;AV;AW;AX;AY;AZ;BA;BB;BC;BD;BE".
                                                                                                                                                                

CREATE "Excel.Application" chExcelApp.

ASSIGN cFileName = "C:\TEMP\TESTE.XLS".

/* Cria a Planilha */
ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar  Visivel */
       chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
       chworksheet            = chExcelapp:sheets:ITEM(1).

/* Incluir Uma Nova Planilha */
ChWorksheet = chWorkbook:Worksheets(1).
chWorkbook:Worksheets:add(,chWorksheet).



/* Seleciona a Workbook */
chWorkbook  = chExcelApp:Workbooks(1).

/* Select a Worksheet */
chWorkbook:Worksheets(1):Activate.
ChWorksheet = chWorkbook:Worksheets(1).

/* Colorindo as Linhas */
DO i-lin = 2 TO 57:
   ASSIGN c-work = STRING(i-lin) + ":" + STRING(i-lin).
   ASSIGN chWorkSheet:Rows(c-work):Interior:ColorIndex   =  (i-lin - 1).
END.

/* Configurando Formatos da Planilha */
chWorkSheet:Range("A2:BE57"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Aell MT"
       chExcelApp:SELECTION:FONT:SIZE = 14
       chExcelApp:SELECTION:NumberFormat = "###.##0".
ASSIGN chworksheet:range("A:BE"):HorizontalAlignment = 3.

/* Display do Numero de Todas as Cores Possiveis das Celulas em Uma Linha */
DO i-lin = 2 TO 57:
   DO i-col = 1 TO 56:
      ASSIGN c-work = ENTRY(i-col,c-col,";") + STRING(i-lin) + ":" + ENTRY(i-col,c-col,";") + STRING(i-lin). 
      ASSIGN chworksheet:range(c-work):VALUE           = i-col
             chworksheet:range(c-work):FONT:ColorIndex = i-col.
   END.
END.

/* Display da Cor da Linha na Coluna "A" */
DO i-lin = 2 TO 57:
   ASSIGN chWorksheet:range("A" + STRING(i-lin)):VALUE = (i-lin - 1).
END.

/* Configurando a Fonte e o Tamanho da Coluna A */
ASSIGN chWorkSheet:COLUMNS("A"):ColumnWidth = 10
       chWorkSheet:COLUMNS("A"):FONT:SIZE   = 16.

/* Congela Celula */
ChWorkbook:Worksheets(1):Activate.
chWorksheet = ChWorkbook:Worksheets(1).
chWorksheet:Range("A2"):SELECT().
chExcelApp:ActiveWindow:freezePanes = TRUE.


/* Salva e Fecha a Planilha */
chExcelApp:DisplayAlerts = FALSE. 
chWorkBook:Save().
chWorkBook:SaveAs(cFileName,,,,,,,).
/* chWorkBook:CLOSE().
   chExcelApp:QUIT(). */
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.
















































