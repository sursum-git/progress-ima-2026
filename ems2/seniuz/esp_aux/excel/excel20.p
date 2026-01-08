/*      Hola lis, 

Gracias por tu soluci¢n, creo que ya he eliminado todos los COM-HANDLE, pero a£n as¡ sigue quedandose el proceso abierto. 

Os adjunto el c¢digo a ver si as¡ alguien sabe que es lo que falla. 

/***********************************************************/ 
*/
DEF VAR chExcelApplication AS COM-HANDLE NO-UNDO. 
DEF VAR chWorkbook AS COM-HANDLE NO-UNDO. 
DEF VAR chWorksheet AS COM-HANDLE NO-UNDO. 

DEF VAR vRow AS DEC NO-UNDO. 
DEF VAR TxtFile AS CHAR NO-UNDO. 
DEF VAR v-pre_tot AS CHARACTER FORMAT "X(15)". 
DEF VAR e-text9 AS CHARACTER FORMAT "X(27)". 

DEF VAR e-text1 AS CHARACTER FORMAT "X(27)". 
DEF VAR e-text2 AS CHARACTER FORMAT "X(27)". 
DEF VAR e-text3 AS CHARACTER FORMAT "X(27)". 
DEF VAR e-text4 AS CHARACTER FORMAT "X(27)". 
DEF VAR e-text5 AS CHARACTER FORMAT "X(27)". 
DEF VAR v-clnif AS CHARACTER FORMAT "X(25)". 
DEF VAR v-imdes AS CHARACTER . 
DEF VAR v-fichero AS CHARACTER FORMAT "X(25)". 

FOR EACH albaran: 

/* c¢digo d¢nde se realizan ciertas operaciones para poder listar los 
albaranes correspondientes */ 

ASSIGN TxtFile = SESSION:TEMP-DIR + STRING(THIS-PROCEDURE) + ".txt":U. 

OUTPUT STREAM Data TO VALUE(txtFile). 

/* m s c¢digo con operaciones y OUTPUT's STREAM Data DELIMITER "~t" para mostrar los resultados */ 

OUTPUT STREAM Data CLOSE. 

CREATE "Excel.Application" chExcelApplication. 

ASSIGN chExcelApplication:Visible = false. 

chExcelApplication:Workbooks:OpenText(txtFile,2,,,,,TRUE). 

ASSIGN chWorkbook = chExcelApplication:WorkBooks:Item(1) 
chWorkSheet = chExcelApplication:Sheets:Item(1). 

RELEASE OBJECT chWorkSheet. 

ASSIGN chWorkSheet = chExcelApplication:Sheets:Item(1). 
chWorkSheet:Rows(1):Insert. 
chWorkSheet:Rows(1):Insert. 
chWorkSheet:Rows(1):Insert. 
chWorkSheet:Rows(1):Insert. 
chWorkSheet:Rows(1):Insert. 
chWorkSheet:Rows(1):Insert. 
chWorkSheet:Rows(1):Insert. 

chWorkSheet:Name = "Albaran". 
chWorksheet:Columns:Font:Size = 7. 
chWorkSheet:StandardWidth = 4.4. 

v-fichero = "C:\ALB" + STRING(albar.alb,"999999") + ".XLS". 


ASSIGN chWorksheet:Range("A1"):Value = "Star Textil s.a." 
chWorksheet:Range("A1"):Font:Size = 16 
chWorksheet:Range("A1"):Font:Bold = True 
chWorksheet:Range("A2"):Value = " A " 
chWorksheet:Range("A3"):Value = " B " 
chWorksheet:Range("A4"):Value = " C " 
chWorksheet:Range("A5"):Value = " D " 
chWorksheet:Range("A6"):Value = " E " 
chWorkSheet:Columns("A"):ColumnWidth = 28 
chWorkSheet:Rows(2):Font:Size = 10 
chWorkSheet:Rows(3):Font:Size = 10 
chWorkSheet:Rows(4):Font:Size = 10 
chWorkSheet:Rows(5):Font:Size = 10 
chWorkSheet:Rows(6):Font:Size = 10 
chWorkSheet:Rows( :Font:Size = 8 
chWorkSheet:Rows(9):Font:Size = 8 
chWorkSheet:Rows(10):Font:Size = 8 
chWorkSheet:Rows(11):Font:Size = 8 
chWorkSheet:Rows(12):Font:Size = 8 
chWorkSheet:Rows(13):Font:Size = 8 
chWorkSheet:Rows(14):Font:Size = 8 
chWorkSheet:Rows(15):Font:Size = 8 
chWorksheet:Range("G8"):Font:Size = 10 
chWorksheet:Range("G9"):Font:Size = 10 
chWorksheet:Range("G10"):Font:Size = 10 
chWorksheet:Range("G11"):Font:Size = 10 
chWorksheet:Range("G12"):Font:Size = 10 
chWorksheet:Range("G13"):Font:Size = 10 
chWorksheet:Range("G14"):Font:Size = 10 
chWorksheet:Range("G15"):Font:Size = 10 
chWorksheet:Range("A9"):Font:Bold = true. 

if chExcelapp:Version begins "8":U THEN
    chWorkBook:SaveAs(v-fichero,39,,,,,,,).
ELSE 
    chWorkBook:SaveAs(v-fichero,,,,,,,).

RELEASE OBJECT chWorkSheet. 
RELEASE OBJECT chWorkBook. 
RELEASE OBJECT chExcelApplication. 

END.
