DEFINE VARIABLE excelAppl   AS COM-HANDLE. 
DEFINE VARIABLE excelSheet  AS COM-HANDLE.

CREATE "Excel.Application" excelAppl.   /* Abre o Excel */

excelAppl:VISIBLE = TRUE. 
excelAppl:WindowState = 1.  /* 1-normal   2-minimizado */ 

excelAppl:Workbooks:ADD("").  /* Cria Nova Planilha */

excelSheet=excelAppl:sheets:ITEM(1). /* Inicializa na Pasta 1 */

excelAppl:worksheets:ITEM(1):SELECT. 
excelAppl:range("A1"):VALUE = STRING(TODAY). 
excelSheet:NAME="EXEMPLO".

excelAppl:worksheets:ITEM(2):SELECT. 
excelAppl:range("B1"):VALUE = STRING(TODAY + 10). 

excelAppl:worksheets:ITEM(3):SELECT. 
excelAppl:range("C1"):VALUE = STRING(TODAY + 20). 

excelAppl:worksheets:ITEM(1):SELECT. 


RELEASE OBJECT excelAppl. 
RELEASE OBJECT excelSheet.

