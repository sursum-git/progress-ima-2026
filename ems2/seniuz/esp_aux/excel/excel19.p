/*
I am working with a COM-HANDLE on Excel (2000).

Some Excel's methods or attributes are using Arrays

Is it possible to pass one of this array to the Excel COM-HANDLE from 4GL???

Example:

=================================
*/

DEFINE VARIABLE chExcelApplication AS COM-HANDLE.
DEFINE VARIABLE chWorkbook AS COM-HANDLE.
DEFINE VARIABLE chWorksheet AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange AS COM-HANDLE.
define variable chQueryTable as com-handle.

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = true.
chExcelApplication:ScreenUpdating = true.
chWorkbook = chExcelApplication:Workbooks:add.
chWorkSheet = chExcelApplication:Sheets:Item(1).
chWorkSheet:name = "Analyse":U.

chWorksheetRange = chWorksheet:range("A1").
chQueryTable = chWorkSheet:QueryTables:ADD("TEXT;C:\TEMP\AA.TXT", chWorksheetRange). 
assign
chQueryTable:name = 'STA_SKU_STO_PER'
chQueryTable:FieldNames = True
chQueryTable:RowNumbers = False
chQueryTable:FillAdjacentFormulas = False
chQueryTable:PreserveFormatting = True
chQueryTable:RefreshOnFileOpen = False
chQueryTable:RefreshStyle = 1 /*xlInsertDeleteCells*/
chQueryTable:SavePassword = False
chQueryTable:SaveData = True
chQueryTable:AdjustColumnWidth = True
chQueryTable:RefreshPeriod = 0
chQueryTable:TextFilePromptOnRefresh = False
chQueryTable:TextFilePlatform = 2 /*xlWindows*/
chQueryTable:TextFileStartRow = 1
chQueryTable:TextFileParseType = 1 /*xlDelimited*/
chQueryTable:TextFileTextQualifier = 1
chQueryTable:TextFileConsecutiveDelimiter = False
chQueryTable:TextFileTabDelimiter = True
chQueryTable:TextFileSemicolonDelimiter = False
chQueryTable:TextFileCommaDelimiter = False
chQueryTable:TextFileSpaceDelimiter = False
.
/*chQueryTable:TextFileColumnDataTypes = ??????? .*/

chQueryTable:Refresh(False) .
/*chExcelApplicationisplayAlerts = false. */
chWorksheet:saveAs(substitute('c:\temp\aaa':U), -4143) no-error. 

message 'End' VIEW-AS ALERT-BOX.

chExcelApplication:ScreenUpdating = true.
chExcelApplication:Visible = true.

RELEASE OBJECT chWorksheet no-error.
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chExcelApplication. 
release object chWorksheetRange no-error.
release object chQueryTable no-error.

