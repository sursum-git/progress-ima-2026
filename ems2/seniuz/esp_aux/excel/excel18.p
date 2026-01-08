/*
cesaroll
23 May 2003, 10:14 AM
Excellent, thank you very much, it works very good.

I just have a question, what I'm doing is creating a simple report, When I send it to a .txt file it works very fast, but if I send it to Excel it works very very slow, I don't know why?, maybe Progress and COM don't work very fine together.

This is my code:
*/



DEFINE VARIABLE chExcel AS COM-HANDLE.
DEFINE VARIABLE chWorkbook AS COM-HANDLE.
DEFINE VARIABLE chWorksheet AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange AS COM-HANDLE.
DEFINE VARIABLE cRen AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRen AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO ON ERROR UNDO, RETRY: 
IF RETRY THEN
DO:
IF VALID-HANDLE(chExcel) THEN 
RELEASE OBJECT chExcel NO-ERROR. 
IF VALID-HANDLE(chWorkbook) then 
RELEASE OBJECT chWorkbook NO-ERROR. 
IF VALID-HANDLE(chWorksheet) then 
RELEASE OBJECT chWorksheet NO-ERROR. 
IF VALID-HANDLE(chWorksheetRange) then 
RELEASE OBJECT chWorksheetRange NO-ERROR. 

ASSIGN chExcel = ? 
chWorkbook = ? 
chWorksheet = ? 
chWorksheetRange = ?. 
RETURN. 
END.
END.

/* CREA Excel Application object */
CREATE "Excel.Application" chExcel.
/* CREA EL Workbook */
chWorkbook = chExcel:Workbooks:Add().

FOR EACH ttPerfiles
NO-LOCK: 

/* Create Worksheet */

ASSIGN chWorkSheet = chExcel:Sheets:Add().

chWorkSheet:NAME = "P-" + STRING(ttPerfiles.iCvePerfil).
chWorkSheet:Range("A3:G8"):FONT:Name = "Comic Sans MS".

/* Configure Columns*/

chWorkSheet:Columns("A"):ColumnWidth = 46.57. 
chWorkSheet:Columns("B"):ColumnWidth = 8.43. 
chWorkSheet:Columns("C"):ColumnWidth = 19.57. 
chWorkSheet:Columns("D"):ColumnWidth = 8.14. 
chWorkSheet:Columns("E"):ColumnWidth = 8.14.
chWorkSheet:Columns("F"):ColumnWidth = 8.14.
chWorkSheet:Columns("G"):ColumnWidth = 13.43.
chWorkSheet:Columns("B"):HorizontalAlignment = 3.
chWorkSheet:Columns("C"):HorizontalAlignment = 3.
chWorkSheet:Columns("D"):HorizontalAlignment = 3.
chWorkSheet:Columns("E"):HorizontalAlignment = 3.
chWorkSheet:Columns("F"):HorizontalAlignment = 3.
chWorkSheet:Columns("G"):HorizontalAlignment = 3.

chWorkSheet:Range("B2"):Value = "User Profile".
chWorkSheet:Range("B2"):FONT:Bold = TRUE.
chWorkSheet:Range("B2"):FONT:SIZE = 18.


chWorkSheet:Range("A4"):Value = "Profile Code:".
chWorkSheet:Range("B4"):Value = STRING(ttPerfiles.iCvePerfil).
chWorkSheet:Range("B4"):Interior:ColorIndex = 35.
chWorkSheet:Range("B4"):FONT:Bold = TRUE.
DO i = 7 TO 10:
chWorkSheet:Range("B4"):Borders(i):LineStyle = 1.
END.
chWorkSheet:Range("A5"):Value = "Profile Description:".
chWorkSheet:Range("C5"):Value = STRING(ttPerfiles.cDescPerfil).
chWorkSheet:Range("B5:F5"):Interior:ColorIndex = 35.
chWorkSheet:Range("C5"):FONT:Bold = TRUE.
DO i = 7 TO 10:
chWorkSheet:Range("B5:F5"):Borders(i):LineStyle = 1.
END.
chWorkSheet:Range("A6"):Value = "Definition Date:".
chWorkSheet:Range("B6:C6"):Interior:ColorIndex = 35.
chWorkSheet:Range("B6:C6"):FONT:Bold = TRUE.
DO i = 7 TO 10:
chWorkSheet:Range("B6:C6"):Borders(i):LineStyle = 1.
END.
chWorkSheet:Range("A7"):Value = "Authorized By:".
chWorkSheet:Range("B7:F7"):Interior:ColorIndex = 35.
chWorkSheet:Range("B7:F7"):FONT:Bold = TRUE.
DO i = 7 TO 10:
chWorkSheet:Range("B7:F7"):Borders(i):LineStyle = 1.
END.


chWorkSheet:Range("A9"):Value = "Description".
chWorkSheet:Range("B9"):Value = "Shortcut".
chWorkSheet:Range("C9"):Value = "C".
chWorkSheet:Range("D9"):Value = "C / M".
chWorkSheet:Range("E9"):Value = "C / M / I".
chWorkSheet:Range("F9"):Value = "C / M / B".
chWorkSheet:Range("G9"):Value = "C / M / I / B".
chWorkSheet:Range("A9:G9"):FONT:Bold = TRUE.
chWorkSheet:Range("A9:G9"):FONT:SIZE = 8.

DO i = 7 TO 11:
chWorkSheet:Range("A9:G9"):Borders(i):LineStyle = 1.
END.

iRen = 10.
FOR EACH ttMenu
NO-LOCK:
cRen = STRING(iRen).
chWorkSheet:Range("A" + cRen + ":G" + cRen):FONT:Name = "Comic Sans MS".

DO i = 7 TO 11:
chWorkSheet:Range("A" + cRen + ":G" + cRen):Borders(i):LineStyle = 1.
END.

IF ttMenu.lHijos THEN
chWorkSheet:Range("A" + cRen):FONT:Bold = TRUE.
chWorkSheet:Range("A" + cRen):Value = ttMenu.cDescMenu.
IF ttMenu.iNivel > 0 THEN
chWorkSheet:Range("A" + cRen):InsertIndent(ttMenu.iNivel).
chWorkSheet:Range("B" + cRen):Value = ttMenu.iCveMenu.
chWorkSheet:Range("C" + cRen + ":G" + cRen):FONT:Bold = TRUE.
chWorkSheet:Range("C" + cRen + ":G" + cRen):Interior:ColorIndex = 35.
FIND FIRST ttPermisos
WHERE ttPermisos.iCveMenu = ttMenu.iCveMenu
AND ttPermisos.iCvePerfil = ttPerfiles.iCvePerfil
NO-LOCK NO-ERROR.
IF AVAILABLE ttPermisos THEN DO:
CASE ttPermisos.iCveAttrib:
WHEN 1 THEN
cRen = "C" + cRen.
WHEN 2 THEN
cRen = "D" + cRen.
WHEN 3 THEN
cRen = "E" + cRen.
WHEN 4 THEN
cRen = "F" + cRen.
WHEN 5 THEN
cRen = "G" + cRen.
OTHERWISE
cRen = ''.
END CASE.

IF cRen <> '' THEN
chWorkSheet:Range(cRen):Value = "x".
END.

iRen = iRen + 1.

END. /* FOR EACH ttMenu */ 

RELEASE OBJECT chWorkSheet.
ASSIGN chWorkSheet = ?.

END. /* FOR EACH ttPerfiles */ 




ASSIGN chExcel:Visible = TRUE.

/* RELEASE COM-HANDLES */
RELEASE OBJECT chExcel NO-ERROR. 
RELEASE OBJECT chWorkbook NO-ERROR.
/*IF VALID-HANDLE(chWorksheet) THEN 
RELEASE OBJECT chWorksheet NO-ERROR.*/
RELEASE OBJECT chWorksheetRange NO-ERROR.

