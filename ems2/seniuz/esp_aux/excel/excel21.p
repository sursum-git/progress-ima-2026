/*******************************************************************************
* Program: tt2xls.i                                                            *
* Purpose: Create a new Excel File based on a static Temp-Table                *
* Author : Jan Sutjahjo                                                        *
* Example of a Program:                                                        *
*   __________________________________________________________________________ * 
*   DEFINE TEMP-TABLE ttDemo NO-UNDO                                           *
*     FIELD FirstName        AS CHARACTER                                      *
*     FIELD LastName         AS CHARACTER                                      *
*     FIELD Amount           AS DECIMAL.                                       *
*                                                                              *
*   CREATE ttDemo.                                                             *
*   ASSIGN                                                                     *
*     ttDemo.FirstName = 'Jan'                                                 *
*     ttDemo.LastName  = 'Sutjahjo'                                            *
*     ttDemo.Amount    = 500.00.                                               *
*                                                                              *
*   RUN pTT2XLS                                                                *
*     ( INPUT TEMP-TABLE ttDemo:DEFAULT-BUFFER-HANDLE,                         *
*       INPUT 'c:\temp\a.xls',                                                 *
*       INPUT 'PageSetup:PrintGridlines=Y|PageSetup:PrintTitleRows=$1:$1' ).   *
*   __________________________________________________________________________ *
*                                                                              *
* Example of Property Parameter Values (last parameter):                       *
*   Font:Name=courier                   - set the font name (def:Arial Narrow) *
*   PageSetup:PrintTitleRows=$1:$1      - set the first row as the title       *
*   PageSetup:CenterFooter=&P of &N     - prt (page#) of (totalpage) in footer *
*   PageSetup:Orientation=2             - set to landscape                     *
*   Visible=Y                           - excel is visible while processing it *
*******************************************************************************/



PROCEDURE pTT2XLS:
/*------------------------------------------------------------------------------
Purpose: Create a new Excel File based on a Static Temp-Table
Notes  : picProperties parameter is name-value-pair with pipe '|' delimiter
------------------------------------------------------------------------------*/

  /*--- parameter definitions ---*/
  DEFINE INPUT PARAMETER pihTT             AS HANDLE                    NO-UNDO.
  DEFINE INPUT PARAMETER picExcelFileName  AS CHARACTER                 NO-UNDO.
  DEFINE INPUT PARAMETER picProperties     AS CHARACTER                 NO-UNDO.

  /*--- local variable definitions ---*/
  DEFINE VARIABLE cColList                 AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRange                   AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cRow                     AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPropEntry               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPropName                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cPropValue               AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE cFontName                AS CHARACTER                 NO-UNDO.
  DEFINE VARIABLE chExcelApplication       AS COM-HANDLE                NO-UNDO.
  DEFINE VARIABLE chWorkbook               AS COM-HANDLE                NO-UNDO.
  DEFINE VARIABLE chWorkSheet              AS COM-HANDLE                NO-UNDO.
  DEFINE VARIABLE hqTT                     AS HANDLE                    NO-UNDO.
  DEFINE VARIABLE iRow                     AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iCol                     AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iPropNo                  AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iPos                     AS INTEGER                   NO-UNDO.
  DEFINE VARIABLE iFontSize                AS INTEGER                   NO-UNDO.

  /*--- validations ---*/
  IF NOT VALID-HANDLE(pihTT) THEN
    RETURN.

  /*--- create a new excel file ---*/
  CREATE "Excel.Application" chExcelApplication.
  chExcelApplication:Visible = FALSE.
  chWorkbook = chExcelApplication:Workbooks:Add().
  chWorkSheet = chExcelApplication:Sheets:Item(1).
  chWorkSheet:Name = pihTT:NAME.


  /*--- set the excel properties ---*/
  DO iPropNo = 1 TO NUM-ENTRIES(picProperties, '|'):

    ASSIGN
      cPropEntry = ENTRY(iPropNo, picProperties, '|')
      iPos       = INDEX(cPropEntry, '=')
      cPropName  = SUBSTRING(cPropEntry, 1, iPos - 1)
      cPropValue = IF iPos > 0 THEN SUBSTRING(cPropEntry, iPos + 1) ELSE ''.

    CASE cPropName:
      WHEN 'Font:Name' THEN ASSIGN cFontName = cPropValue.
      WHEN 'Font:Size' THEN ASSIGN iFontSize = INTEGER(cPropValue) NO-ERROR.
      WHEN 'PageSetup:Orientation' THEN
        chWorkSheet:PageSetup:Orientation = INTEGER(cPropValue) NO-ERROR.
      WHEN 'PageSetup:Zoom' THEN
        chWorkSheet:PageSetup:Zoom = cPropValue.
      WHEN 'PageSetup:PrintGridlines' THEN
        chWorkSheet:PageSetup:PrintGridlines = CAN-DO('YES,TRUE,Y,T',cPropValue).
      WHEN 'PageSetup:PrintTitleRows' THEN
        chWorkSheet:PageSetup:PrintTitleRows = cPropValue.
      WHEN 'PageSetup:PrintTitleColumns' THEN
        chWorkSheet:PageSetup:PrintTitleColumns = cPropValue.
      WHEN 'PageSetup:LeftHeader' THEN
        chWorkSheet:PageSetup:LeftHeader = cPropValue.
      WHEN 'PageSetup:CenterHeader' THEN
        chWorkSheet:PageSetup:CenterHeader = cPropValue.
      WHEN 'PageSetup:RightHeader' THEN
        chWorkSheet:PageSetup:RightHeader = cPropValue.
      WHEN 'PageSetup:LeftFooter' THEN
        chWorkSheet:PageSetup:LeftFooter = cPropValue.
      WHEN 'PageSetup:CenterFooter' THEN
        chWorkSheet:PageSetup:CenterFooter = cPropValue.
      WHEN 'PageSetup:RightFooter' THEN
        chWorkSheet:PageSetup:RightFooter = cPropValue.
      WHEN 'PageSetup:CenterHorizontally' THEN
        chWorkSheet:PageSetup:CenterHorizontally = CAN-DO('YES,TRUE,Y,T',cPropValue).
      WHEN 'PageSetup:CenterVertically' THEN
        chWorkSheet:PageSetup:CenterVertically = CAN-DO('YES,TRUE,Y,T',cPropValue).
      WHEN 'PageSetup:FitToPagesWide' THEN
        chWorkSheet:PageSetup:FitToPagesWide = INTEGER(cPropValue) NO-ERROR.
      WHEN 'PageSetup:FitToPagesTall' THEN
        chWorkSheet:PageSetup:FitToPagesTall = INTEGER(cPropValue) NO-ERROR.
      WHEN 'Visible' THEN
        chExcelApplication:Visible = CAN-DO('YES,TRUE,Y,T',cPropValue).
    END CASE. /* cPropName */

  END. /* DO iPropNo = 1 TO NUM-ENTRIES(picProperties): */

  IF cFontName = '' THEN
    ASSIGN cFontName = "Arial Narrow".

  
  /*--- set the column attributes for the Worksheet ---*/
  ASSIGN cColList = 'A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z'
                  + ',AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM'
                  + ',AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ'
                  + ',BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM'
                  + ',BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ'
                  + ',CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM'
                  + ',CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ'
                  + ',DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM'
                  + ',DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ'
                  + ',EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM'
                  + ',EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ'
                  + ',FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM'
                  + ',FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ'
                  + ',GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM'
                  + ',GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ'.
             
  chWorkSheet:Rows("1:1"):Font:Bold = TRUE.
  chWorkSheet:Rows("2:2"):Activate.
  chExcelApplication:ActiveWindow:FreezePanes = TRUE.

  DO iCol = 1 TO pihTT:NUM-FIELDS:
    chWorkSheet:Range(ENTRY(iCol,cColList) + "1"):Value = pihTT:BUFFER-FIELD(iCol):LABEL.
    chWorkSheet:Columns(ENTRY(iCol,cColList)):Font:Name = cFontName.
    IF iFontSize > 0 THEN
      chWorkSheet:Columns(ENTRY(iCol,cColList)):Font:Size = iFontSize.
    IF pihTT:BUFFER-FIELD(iCol):DATA-TYPE = "DECIMAL" THEN
      chWorkSheet:Columns(ENTRY(iCol,cColList)):Cells:NumberFormat = "#,###,##0.00".
    ELSE IF pihTT:BUFFER-FIELD(iCol):DATA-TYPE = "CHARACTER" THEN
      chWorkSheet:Columns(ENTRY(iCol,cColList)):Cells:NumberFormat = "@".
  END. /* DO iCol = 1 TO pihTT:NUM-FIELDS: */


  /*--- set the query ---*/
  CREATE QUERY hqTT.
  hqTT:SET-BUFFERS(pihTT).
  hqTT:QUERY-PREPARE("FOR EACH " + pihTT:NAME).
  hqTT:QUERY-OPEN.

  ASSIGN iRow = 1.
  REPEAT:
    hqTT:GET-NEXT.
    IF hqTT:QUERY-OFF-END THEN LEAVE.
    ASSIGN iRow = iRow + 1
           cRow = STRING(iRow).
    DO iCol = 1 TO pihTT:NUM-FIELDS:
      chWorkSheet:Range(ENTRY(iCol,cColList) + cRow):Value = pihTT:BUFFER-FIELD(iCol):BUFFER-VALUE.
    END.
  END. /* REPEAT: */

  hqTT:QUERY-CLOSE.
  DELETE OBJECT hqTT.


  /*--- set the column width automatically ---*/
  DO iCol = 1 TO pihTT:NUM-FIELDS:
    chWorkSheet:Columns(ENTRY(iCol,cColList)):AutoFit().
  END. /* DO iCol = 1 TO pihTT:NUM-FIELDS: */


  /*--- save the result file in excel format ---*/
  IF picExcelFileName > '' THEN DO:
    chWorkBook:SaveCopyAs(picExcelFileName).
    chWorkBook:Close(NO).
    chExcelApplication:Quit().
  END.

  RELEASE OBJECT chExcelApplication NO-ERROR.
  RELEASE OBJECT chWorkbook NO-ERROR.
  RELEASE OBJECT chWorksheet NO-ERROR.


END PROCEDURE. /* pTT2XLS */

