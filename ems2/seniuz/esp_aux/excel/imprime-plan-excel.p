/*Abre uma planilha, altera algum valor e imprime...*/

DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF var chWorksheet AS COM-HANDLE NO-UNDO.
DEF VAR cFileName   AS CHAR.
DEF VAR i-Lin       AS INT INITIAL 2.
DEF VAR c-faixa     AS CHAR.
DEF VAR c-nota      AS CHAR.

CREATE "Excel.Application" chExcelApp.

ASSIGN cFileName = "C:\TEMP\TESTE.xls".

/* Abre a Planilha */
chExcelApp:VISIBLE = FALSE.  /* A Planilha Ficar  Visivel */
chWorkbook         = chExcelApp:Workbooks:OPEN(cFileName).
chWorkSheet        = chExcelApp:Sheets:Item(1).

chWorkSheet:Range("P10:Q258"):NumberFormat = "@".
DO i-Lin = 10 TO 258:
   ASSIGN c-nota = chWorksheet:range("G" + STRING(i-Lin)):VALUE.
   FIND nota-fiscal WHERE nota-fiscal.cod-estabel = "2"
                      AND nota-fiscal.serie       = "1"
                      AND nota-fiscal.nr-nota-fis = "0" + SUBSTR(c-nota,1,6)
                    NO-LOCK NO-ERROR.
   IF AVAIL nota-fiscal THEN
      ASSIGN chWorksheet:range("P" + STRING(i-Lin)):VALUE = STRING(nota-fiscal.dt-emis-nota,"99/99/9999")
             chWorksheet:range("Q" + STRING(i-Lin)):VALUE = STRING(nota-fiscal.dt-saida,"99/99/9999").
END.

/* Salva e Imprime e Fecha a Planilha */
chExcelApp:DisplayAlerts = FALSE. 
/*chExcelApp:ActivePrinter = "\\anuket\LexE450d-info".*/
chWorkBook:Save().
chWorkBook:printOut().


/*objApp.Worksheets(1).Application.ActivePrinter */


/*chWorkBook:SaveAs(cFileName,,,,,,,).*/
chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.
