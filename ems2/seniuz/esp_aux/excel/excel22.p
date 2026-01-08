v-cnt1 = 0.  /* counter variable to loop through textfile names */
v-cnt = 0.
iColumn = 6. /* starting row to display data on the excel sheet. */



FOR EACH t-part /* USE-INDEX i-locitem */
                BREAK BY t-part.st-loc
                      BY t-part.category:


  IF FIRST-OF(t-part.st-loc)
  THEN
  DO:
      v-loc-cnt = v-loc-cnt + 1.  /* counter variable for looping thru excelfile names */
      v-sheet-cnt = 1.  /* initialise sheet count to start at one for new workbooks. */

      /* release handles just incase there are any left hanging around */

      IF VALID-HANDLE(chBorder) THEN RELEASE OBJECT chBorder.
      IF VALID-HANDLE(chQueryTable) THEN RELEASE OBJECT chQueryTable.
      IF VALID-HANDLE(chWorksheet) THEN RELEASE OBJECT chWorksheet.
      IF VALID-HANDLE(chWorkbook) THEN RELEASE OBJECT chWorkbook.
      IF VALID-HANDLE(chExcelApplication) THEN RELEASE OBJECT chExcelApplication.

      ASSIGN
          chBorder = ?
          chWorksheet = ?
          chWorkbook = ?
          chExcelApplication = ?.


      /* create excel application */
      create "Excel.Application" chExcelApplication.
      chExcelApplication:VISIBLE = FALSE.
      chWorkbook = chExcelApplication:Workbooks:Add().

      chExcelApplication:DisplayAlerts = FALSE.

  END.


  /* for each category, open up text file created, insert into sheet, format and save as specified excel file name */

      IF FIRST-OF(t-part.category)
      THEN
      DO:

          FIND flextable WHERE flextable.type = INT(t-part.category)
              AND flextable.code = 0
              NO-LOCK NO-ERROR.

          v-cnt1 = v-cnt1 + 1.


          IF VALID-HANDLE(chQueryTable) THEN RELEASE OBJECT chQueryTable.

          RUN OpenSheet(chExcelApplication,INPUT-OUTPUT chWorksheet,v-sheet-cnt,TRIM(SUBSTRING(flextable.codedesc,1,10))).

          chQueryTable = chWorksheet:QueryTables:Add("TEXT;" + v-textfile[v-cnt1],chWorksheet:Cells(5,1)).

          chQueryTable:REFRESH().

          IF VALID-HANDLE(chQueryTable) THEN RELEASE OBJECT chQueryTable.


          cColumn = STRING(iColumn).
          cRange = "A" + cColumn + ":I" + cColumn.

          chWorkSheet:Range(cRange):RowHeight = 50.
          chWorkSheet:Range("A:AF"):HorizontalAlignment = -4108. /*center */
          chWorkSheet:Range("A:AF"):VerticalAlignment = -4108. /*center */

          cColumn = STRING(iColumn).
          cRange = "A" + cColumn + ":AA" + cColumn.

          chWorkSheet:Range(cRange):WrapText = TRUE.

          cColumn = STRING(iColumn).
          cRange = "A1" + ":AF" + cColumn.

          chWorkSheet:Range(cRange):Font:Bold = TRUE.

          /* v-col[v-cnt1] - stores the number of columns on a particular sheet */
          /* v-row[v-cnt1] - stores the number of rows on a particular sheet. Includes heading so remember to 
             subtract one when calculating formulae. */



          cColumn = STRING(iColumn).
          cRange = "A" + cColumn + ":".
          cRange = cRange + ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(v-row[v-cnt1] + iColumn - 1).


          chBorder = chWorkSheet:Range(cRange).
          chBorder:Borders(7):LineStyle = 1. /* Left border */
          chBorder:Borders(8):LineStyle = 1. /* Top border */
          chBorder:Borders(9):LineStyle = 1. /* Bottom border */
          chBorder:Borders(10):LineStyle = 1. /* Right border */
          chBorder:Borders(11):LineStyle = 1. /* Inside Vertical border */
          chBorder:Borders(12):LineStyle = 1. /* Inside Horizontal border */

          IF VALID-HANDLE(chBorder) THEN RELEASE OBJECT chBorder.



          cRange = "B1:I1".
/*           cRange = cRange + ENTRY(v-col[v-cnt1],v-cell-letter) + "1".  */

          chWorkSheet:Range(cRange):MergeCells = TRUE.

          chWorkSheet:Range("A5"):COPY.
          chWorkSheet:Range("B1"):PasteSpecial(-4123,-4142,FALSE,FALSE).
          chWorkSheet:Range("B1"):FONT:UNDERLINE = 2.
          chWorkSheet:Range("B1"):FONT:SIZE = "12".
          chWorkSheet:Range("A5"):VALUE = "".

          cRange = "B2:I2".
/*           cRange = cRange + ENTRY(v-col[v-cnt1],v-cell-letter) + "2".  */

          chWorkSheet:Range(cRange):MergeCells = TRUE.

          chWorkSheet:Range("B5"):COPY.
          chWorkSheet:Range("B2"):PasteSpecial(-4123,-4142,FALSE,FALSE).
          chWorkSheet:Range("B2"):FONT:UNDERLINE = 2.
          chWorkSheet:Range("B2"):FONT:SIZE = "12".
          chWorkSheet:Range("B5"):VALUE = "".

          chWorkSheet:Range("A4"):VALUE = "DATE:".
          chWorkSheet:Range("B4"):VALUE = STRING(ENTRY(INT(v-period), v-months)) + " " + string(v-year).


          /* formulae for calcuating totals */

          /* total quantity column */
          IF v-row[v-cnt1] > 1 THEN DO:
              
              /* first cell to add the formula */
              cRange = ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(iColumn + 1).

              /* range of cells to add up */
              bRange = "B" + STRING(iColumn + 1) + ":" + ENTRY((v-col[v-cnt1] - 3),v-cell-letter) + STRING(iColumn + 1).

              /* add fromula */
              chWorkSheet:Range(cRange):formula = "=SUM(" + bRange + ")".

          END.


          IF v-row[v-cnt1] > 2 THEN DO:

              /* copy formula */
              chWorkSheet:Range(cRange):COPY.

             /* range of cells below the  cell with formula */
             cRange = ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(iColumn + 2) + ":".

            cRange = cRange + ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(v-row[v-cnt1] + (iColumn - 1)).

             /* paste formula */
            chWorkSheet:Range(cRange):PasteSpecial(-4123,-4142,FALSE,FALSE).

          END.


          /* o/all cost column */
          IF v-row[v-cnt1] > 1 THEN DO:
              
              /* first cell to add the formula */
              cRange = ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(iColumn + 1).

              /* cells to multiply up */
              bRange = ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(iColumn + 1) + "*" + ENTRY((v-col[v-cnt1] - 1),v-cell-letter) + STRING(iColumn + 1).

              /* add fromula */
              chWorkSheet:Range(cRange):formula = "=SUM(" + bRange + ")".

          END.


          IF v-row[v-cnt1] > 2 THEN DO:

              /* copy formula */
              chWorkSheet:Range(cRange):COPY.

             /* range of cells below the  cell with formula */
             cRange = ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(iColumn + 2) + ":".

            cRange = cRange + ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(v-row[v-cnt1] + (iColumn - 1)).

             /* paste formula */
            chWorkSheet:Range(cRange):PasteSpecial(-4123,-4142,FALSE,FALSE).

          END.


          IF v-row[v-cnt1] > 1 THEN DO:

          /* grand total */


          cRange = ENTRY((v-col[v-cnt1] - 4),v-cell-letter) + STRING(v-row[v-cnt1] + iColumn) + ":".
          cRange = cRange + ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(v-row[v-cnt1] + iColumn).
          
          chBorder = chWorkSheet:Range(cRange).
          chBorder:Borders(7):LineStyle = 1. /* Left border */
          chBorder:Borders(8):LineStyle = 1. /* Top border */
          chBorder:Borders(9):LineStyle = 1. /* Bottom border */
          chBorder:Borders(10):LineStyle = 1. /* Right border */
          chBorder:Borders(11):LineStyle = 1. /* Inside Vertical border */

          IF VALID-HANDLE(chBorder) THEN RELEASE OBJECT chBorder.



          chWorkSheet:Range(cRange):HorizontalAlignment = -4108. /*center */
          chWorkSheet:Range(cRange):VerticalAlignment = -4108. /*center */

          chWorkSheet:Range(cRange):Font:Bold = TRUE.


          cRange = ENTRY((v-col[v-cnt1] - 4),v-cell-letter) + STRING(v-row[v-cnt1] + iColumn) + ":".
          cRange = cRange + ENTRY((v-col[v-cnt1] - 3),v-cell-letter) + STRING(v-row[v-cnt1] + iColumn).


          chWorkSheet:Range(cRange):MergeCells = TRUE.


          cRange = ENTRY((v-col[v-cnt1] - 4),v-cell-letter) + STRING(v-row[v-cnt1] + iColumn).
          
          chWorkSheet:Range(cRange):VALUE = "GRAND TOTAL".


          cRange = ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(v-row[v-cnt1] + iColumn).

          bRange = ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(iColumn + 1) + ":".

          bRange = bRange + ENTRY((v-col[v-cnt1] - 2),v-cell-letter) + STRING(v-row[v-cnt1] + (iColumn - 1)).

           /* add fromula */
          chWorkSheet:Range(cRange):formula = "=SUM(" + bRange + ")".




          cRange = ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(v-row[v-cnt1] + iColumn).

          bRange = ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(iColumn + 1) + ":".

          bRange = bRange + ENTRY(v-col[v-cnt1],v-cell-letter) + STRING(v-row[v-cnt1] + (iColumn - 1)).

           /* add fromula */
          chWorkSheet:Range(cRange):formula = "=SUM(" + bRange + ")".


          END.

          IF v-row[v-cnt1] > 2 THEN
          DO:
              
              chWorkSheet:Rows("7"):DELETE(-4162). 
          END.


          cRange = "A:".
          cRange = cRange + ENTRY(v-col[v-cnt1],v-cell-letter).
          chWorkSheet:COLUMNS(cRange):ColumnWidth = 16.

          chExcelApplication:ActiveWindow:Zoom = 75.

          chWorkSheet:Range("B7"):SELECT.

          chExcelApplication:ActiveWindow:FreezePanes = TRUE.

/*           chExcelApplication:ActiveSheet:PageSetup:Zoom = 60.       */
/*           chExcelApplication:ActiveSheet:PageSetup:ORIENTATION = 2. */

          v-sheet-cnt = v-sheet-cnt + 1.

     END.

          IF LAST-OF(t-part.st-loc)
          THEN
          DO:

            RUN OpenSheet(chExcelApplication,INPUT-OUTPUT chWorksheet,1, "").

            chExcelApplication:DisplayAlerts = FALSE.

            chWorkbook:SaveAs (v-excelfile[v-loc-cnt],-4143,,,,,).

            chExcelApplication:Workbooks:CLOSE().

            chExcelApplication:QUIT ().


            IF VALID-HANDLE(chBorder) THEN RELEASE OBJECT chBorder.
            IF VALID-HANDLE(chQueryTable) THEN RELEASE OBJECT chQueryTable.
            IF VALID-HANDLE(chWorksheet) THEN RELEASE OBJECT chWorksheet.
            IF VALID-HANDLE(chWorkbook) THEN RELEASE OBJECT chWorkbook.
            IF VALID-HANDLE(chExcelApplication) THEN RELEASE OBJECT chExcelApplication.

            ASSIGN
                chBorder = ?
                chWorksheet = ?
                chWorkbook = ?
                chExcelApplication = ?.

          END.


END.


PROCEDURE OpenSheet:
DEF INPUT PARAM phExcel AS COM-HANDLE NO-UNDO.
DEF INPUT-OUTPUT PARAM phExcelWorkSheet AS COM-HANDLE NO-UNDO.
DEF INPUT PARAM piSheetNum AS INT.
DEF INPUT PARAM pcategory AS CHAR NO-UNDO.

DEF VAR iNumSheets AS INT NO-UNDO.

IF VALID-HANDLE(phExcelWorkSheet)
THEN RELEASE OBJECT phExcelWorkSheet.


iNumSheets = phExcel:sheets:COUNT.

IF iNumSheets >= piSheetNum
THEN phExcelWorkSheet = phExcel:sheets:ITEM(piSheetNum).
ELSE DO:
phExcelWorkSheet = phExcel:sheets:ITEM(iNumSheets).
phExcelWorkSheet:SELECT.
phExcelWorkSheet = phExcel:Sheets:ADD.
END.

phExcelWorkSheet:SELECT.

IF pcategory <> "" THEN
phExcelWorkSheet:NAME = pcategory.

END PROCEDURE.
