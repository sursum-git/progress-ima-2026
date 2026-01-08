DEF VAR chExcelApp       AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook       AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheet      AS COM-HANDLE NO-UNDO.
DEF VAR chWorksheetRange AS COM-HANDLE. 
DEF VAR chChart          AS COM-HANDLE. 
DEF VAR cFileName   AS CHAR.
DEF VAR i-lin       AS INT INITIAL 2.
DEF VAR i-ct        AS INT.


CREATE "Excel.Application" chExcelApp.

ASSIGN cFileName = "C:\TEMP\TESTE.XLS".

/* Cria a Planilha */
ASSIGN chExcelApp:VISIBLE     = FALSE  /* A Planilha NÇO Ficar  Visivel */
       chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
       chworksheet            = chExcelapp:sheets:ITEM(1).

/* Incluir Uma Nova Planilha */
ChWorksheet = chWorkbook:Worksheets(1).
chWorkbook:Worksheets:add(,chWorksheet).


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
ASSIGN chWorksheet:range("B01"):VALUE = "Valor em U$"
       chWorksheet:range("B02"):VALUE = 255
       chWorksheet:range("B03"):VALUE = 100
       chWorksheet:range("B04"):VALUE = 250
       chWorksheet:range("B05"):VALUE = 400
       chWorksheet:range("B06"):VALUE = 100
       chWorksheet:range("B07"):VALUE = 600
       chWorksheet:range("B08"):VALUE = 255
       chWorksheet:range("B09"):VALUE = 100
       chWorksheet:range("B10"):VALUE = 250
       chWorksheet:range("B11"):VALUE = 400
       chWorksheet:range("B12"):VALUE = 100
       chWorksheet:range("B13"):VALUE = 600
       chWorksheet:range("B14"):VALUE = 255
       chWorksheet:range("B15"):VALUE = 100
       chWorksheet:range("B16"):VALUE = 250
       chWorksheet:range("B17"):VALUE = 400
       chWorksheet:range("B18"):VALUE = 100
       chWorksheet:range("B19"):VALUE = 600
       chWorksheet:range("B20"):VALUE = 255
       chWorksheet:range("B21"):VALUE = 100
       chWorksheet:range("B22"):VALUE = 250
       chWorksheet:range("B23"):VALUE = 400
       chWorksheet:range("B24"):VALUE = 100
       chWorksheet:range("B25"):VALUE = 250
       chWorksheet:range("B26"):VALUE = 400
       chWorksheet:range("B27"):VALUE = 100
       chWorksheet:range("B28"):VALUE = 600
       chWorksheet:range("B29"):VALUE = 255
       chWorksheet:range("B30"):VALUE = 100
       chWorksheet:range("B31"):VALUE = 250
       chWorksheet:range("B32"):VALUE = 400
       chWorksheet:range("B33"):VALUE = 100
       chWorksheet:range("B34"):VALUE = 600
       chWorksheet:range("B35"):VALUE = 255
       chWorksheet:range("B36"):VALUE = 100
       chWorksheet:range("B37"):VALUE = 250.

ASSIGN chWorksheet:range("C01"):VALUE = "Valor em R$"
       chWorksheet:range("C02"):VALUE = 155
       chWorksheet:range("C03"):VALUE = 165
       chWorksheet:range("C04"):VALUE = 175
       chWorksheet:range("C05"):VALUE = 185
       chWorksheet:range("C06"):VALUE = 110
       chWorksheet:range("C07"):VALUE = 250
       chWorksheet:range("C08"):VALUE = 255
       chWorksheet:range("C09"):VALUE = 245
       chWorksheet:range("C10"):VALUE = 320
       chWorksheet:range("C11"):VALUE = 235
       chWorksheet:range("C12"):VALUE = 225
       chWorksheet:range("C13"):VALUE = 215
       chWorksheet:range("C14"):VALUE = 202
       chWorksheet:range("C15"):VALUE = 195
       chWorksheet:range("C16"):VALUE = 262
       chWorksheet:range("C17"):VALUE = 272
       chWorksheet:range("C18"):VALUE = 292
       chWorksheet:range("C19"):VALUE = 282
       chWorksheet:range("C20"):VALUE = 302
       chWorksheet:range("C21"):VALUE = 210
       chWorksheet:range("C22"):VALUE = 215
       chWorksheet:range("C23"):VALUE = 220
       chWorksheet:range("C24"):VALUE = 225
       chWorksheet:range("C25"):VALUE = 235
       chWorksheet:range("C26"):VALUE = 233
       chWorksheet:range("C27"):VALUE = 200
       chWorksheet:range("C28"):VALUE = 202
       chWorksheet:range("C29"):VALUE = 204
       chWorksheet:range("C30"):VALUE = 206
       chWorksheet:range("C31"):VALUE = 208
       chWorksheet:range("C32"):VALUE = 210
       chWorksheet:range("C33"):VALUE = 212
       chWorksheet:range("C34"):VALUE = 214
       chWorksheet:range("C35"):VALUE = 216
       chWorksheet:range("C36"):VALUE = 218
       chWorksheet:range("C37"):VALUE = 220.

ASSIGN chWorksheet:range("A01"):VALUE = "Data"
       chWorksheet:range("A02"):VALUE = "01/01/2009"
       chWorksheet:range("A03"):VALUE = "01/02/2009"
       chWorksheet:range("A04"):VALUE = "01/03/2009"
       chWorksheet:range("A05"):VALUE = "01/04/2009"
       chWorksheet:range("A06"):VALUE = "01/05/2009"
       chWorksheet:range("A07"):VALUE = "01/06/2009"
       chWorksheet:range("A08"):VALUE = "01/07/2009"
       chWorksheet:range("A09"):VALUE = "01/08/2009"
       chWorksheet:range("A10"):VALUE = "01/09/2009"
       chWorksheet:range("A11"):VALUE = "01/10/2009"
       chWorksheet:range("A12"):VALUE = "01/11/2009"
       chWorksheet:range("A13"):VALUE = "01/12/2009"
       chWorksheet:range("A14"):VALUE = "01/13/2009"
       chWorksheet:range("A15"):VALUE = "01/14/2009"
       chWorksheet:range("A16"):VALUE = "01/15/2009"
       chWorksheet:range("A17"):VALUE = "01/16/2009"
       chWorksheet:range("A18"):VALUE = "01/17/2009"
       chWorksheet:range("A19"):VALUE = "01/18/2009"
       chWorksheet:range("A20"):VALUE = "01/19/2009"
       chWorksheet:range("A21"):VALUE = "01/20/2009"
       chWorksheet:range("A22"):VALUE = "01/21/2009"
       chWorksheet:range("A23"):VALUE = "01/22/2009"
       chWorksheet:range("A24"):VALUE = "01/23/2009"
       chWorksheet:range("A25"):VALUE = "01/24/2009"
       chWorksheet:range("A26"):VALUE = "01/25/2009"
       chWorksheet:range("A27"):VALUE = "01/26/2009"
       chWorksheet:range("A28"):VALUE = "01/28/2009"
       chWorksheet:range("A29"):VALUE = "01/29/2009"
       chWorksheet:range("A30"):VALUE = "01/30/2009"
       chWorksheet:range("A31"):VALUE = "01/31/2009"
       chWorksheet:range("A32"):VALUE = "02/01/2009"
       chWorksheet:range("A33"):VALUE = "02/02/2009"
       chWorksheet:range("A34"):VALUE = "02/03/2009"
       chWorksheet:range("A35"):VALUE = "02/04/2009"
       chWorksheet:range("A36"):VALUE = "02/05/2009"
       chWorksheet:range("A37"):VALUE = "02/06/2009".

chWorkSheet:Range("B2:B37"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Bell MT"
       chExcelApp:SELECTION:FONT:SIZE = 16
       chExcelApp:SELECTION:NumberFormat = "_([$$-409]* ###.##0,00_);_([$$-409]* (###.##0,00);_([$$-409]* ""-""??_);_(@_)".
/*       chExcelApp:SELECTION:Style = "Currency". */


chWorkSheet:Range("C2:C37"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Consolas"
       chExcelApp:SELECTION:FONT:SIZE = 16
       chExcelApp:SELECTION:Style = "Currency". 

chWorkSheet:Range("A2:A37"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Arial"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 6
       chExcelApp:SELECTION:FONT:ColorIndex = 10.


ASSIGN chWorkSheet:COLUMNS("A"):ColumnWidth = 16
       chWorkSheet:COLUMNS("A"):FONT:SIZE   = 14.

ASSIGN chworksheet:range("A1:A1"):HorizontalAlignment = 4
       chworksheet:range("B1:B1"):HorizontalAlignment = 4
       chworksheet:range("C1:C1"):HorizontalAlignment = 4.

ASSIGN chWorksheet:range("B02"):Interior:ColorIndex = 1
       chWorksheet:range("B02"):FONT:ColorIndex     = 2 
       chWorksheet:range("B03"):Interior:ColorIndex = 3 
       chWorksheet:range("B03"):FONT:ColorIndex     = 4 
       chWorksheet:range("B04"):Interior:ColorIndex = 5 
       chWorksheet:range("B04"):FONT:ColorIndex     = 6 
       chWorksheet:range("B05"):FONT:ColorIndex     = 7 
       chWorksheet:range("B05"):Interior:ColorIndex = 8 
       chWorksheet:range("B06"):FONT:ColorIndex     = 9 
       chWorksheet:range("B06"):Interior:ColorIndex = 10
       chWorksheet:range("B07"):FONT:ColorIndex     = 11
       chWorksheet:range("B07"):FONT:ColorIndex     = 12
       chWorksheet:range("B08"):Interior:ColorIndex = 13
       chWorksheet:range("B08"):FONT:ColorIndex     = 14
       chWorksheet:range("B09"):Interior:ColorIndex = 15
       chWorksheet:range("B09"):FONT:ColorIndex     = 16
       chWorksheet:range("B10"):FONT:ColorIndex     = 17
       chWorksheet:range("B10"):Interior:ColorIndex = 18
       chWorksheet:range("B11"):FONT:ColorIndex     = 19
       chWorksheet:range("B11"):Interior:ColorIndex = 20
       chWorksheet:range("B12"):FONT:ColorIndex     = 21
       chWorksheet:range("B12"):FONT:ColorIndex     = 22
       chWorksheet:range("B13"):Interior:ColorIndex = 23
       chWorksheet:range("B13"):FONT:ColorIndex     = 24
       chWorksheet:range("B14"):Interior:ColorIndex = 25
       chWorksheet:range("B14"):FONT:ColorIndex     = 26
       chWorksheet:range("B15"):FONT:ColorIndex     = 27
       chWorksheet:range("B15"):Interior:ColorIndex = 28
       chWorksheet:range("B16"):FONT:ColorIndex     = 29
       chWorksheet:range("B16"):Interior:ColorIndex = 30
       chWorksheet:range("B17"):FONT:ColorIndex     = 31
       chWorksheet:range("B17"):FONT:ColorIndex     = 32
       chWorksheet:range("B18"):Interior:ColorIndex = 33
       chWorksheet:range("B18"):FONT:ColorIndex     = 34
       chWorksheet:range("B19"):Interior:ColorIndex = 41
       chWorksheet:range("B19"):FONT:ColorIndex     = 36
       chWorksheet:range("B20"):FONT:ColorIndex     = 37
       chWorksheet:range("B20"):Interior:ColorIndex = 38
       chWorksheet:range("B21"):FONT:ColorIndex     = 39
       chWorksheet:range("B21"):Interior:ColorIndex = 40
       chWorksheet:range("B22"):FONT:ColorIndex     = 41
       chWorksheet:range("B22"):Interior:ColorIndex = 42
       chWorksheet:range("B23"):FONT:ColorIndex     = 43
       chWorksheet:range("B23"):Interior:ColorIndex = 44
       chWorksheet:range("B24"):FONT:ColorIndex     = 45
       chWorksheet:range("B24"):Interior:ColorIndex = 46
       chWorksheet:range("B25"):FONT:ColorIndex     = 47
       chWorksheet:range("B25"):Interior:ColorIndex = 48
       chWorksheet:range("B26"):FONT:ColorIndex     = 49
       chWorksheet:range("B26"):Interior:ColorIndex = 50
       chWorksheet:range("B27"):FONT:ColorIndex     = 51
       chWorksheet:range("B27"):Interior:ColorIndex = 52
       chWorksheet:range("B28"):FONT:ColorIndex     = 53
       chWorksheet:range("B28"):Interior:ColorIndex = 54
       chWorksheet:range("B29"):FONT:ColorIndex     = 55
       chWorksheet:range("B29"):Interior:ColorIndex = 56
       chWorksheet:range("B30"):FONT:ColorIndex     = 01
       chWorksheet:range("B30"):Interior:ColorIndex = 20
       chWorksheet:range("B31"):FONT:ColorIndex     = 07
       chWorksheet:range("B31"):Interior:ColorIndex = 20
       chWorksheet:range("B32"):FONT:ColorIndex     = 08
       chWorksheet:range("B32"):Interior:ColorIndex = 20
       chWorksheet:range("B33"):FONT:ColorIndex     = 33
       chWorksheet:range("B33"):Interior:ColorIndex = 20
       chWorksheet:range("B34"):FONT:ColorIndex     = 32
       chWorksheet:range("B34"):Interior:ColorIndex = 20
       chWorksheet:range("B35"):FONT:ColorIndex     = 31
       chWorksheet:range("B35"):Interior:ColorIndex = 20
       chWorksheet:range("B36"):FONT:ColorIndex     = 30
       chWorksheet:range("B36"):Interior:ColorIndex = 20
       chWorksheet:range("B37"):FONT:ColorIndex     = 29
       chWorksheet:range("B37"):Interior:ColorIndex = 20.


ASSIGN chWorksheet:range("C36"):Interior:ColorIndex = 1
       chWorksheet:range("C36"):FONT:ColorIndex     = 2 
       chWorksheet:range("C35"):Interior:ColorIndex = 3 
       chWorksheet:range("C35"):FONT:ColorIndex     = 4 
       chWorksheet:range("C34"):Interior:ColorIndex = 5 
       chWorksheet:range("C34"):FONT:ColorIndex     = 6 
       chWorksheet:range("C33"):FONT:ColorIndex     = 7 
       chWorksheet:range("C33"):Interior:ColorIndex = 8 
       chWorksheet:range("C32"):FONT:ColorIndex     = 9 
       chWorksheet:range("C32"):Interior:ColorIndex = 10
       chWorksheet:range("C31"):FONT:ColorIndex     = 11
       chWorksheet:range("C31"):FONT:ColorIndex     = 12
       chWorksheet:range("C30"):Interior:ColorIndex = 13
       chWorksheet:range("C30"):FONT:ColorIndex     = 14
       chWorksheet:range("C29"):Interior:ColorIndex = 15
       chWorksheet:range("C29"):FONT:ColorIndex     = 16
       chWorksheet:range("C28"):FONT:ColorIndex     = 17
       chWorksheet:range("C28"):Interior:ColorIndex = 18
       chWorksheet:range("C27"):FONT:ColorIndex     = 19
       chWorksheet:range("C27"):Interior:ColorIndex = 20
       chWorksheet:range("C26"):Interior:ColorIndex = 20
       chWorksheet:range("C26"):FONT:ColorIndex     = 22
       chWorksheet:range("C25"):Interior:ColorIndex = 23
       chWorksheet:range("C25"):FONT:ColorIndex     = 24
       chWorksheet:range("C24"):Interior:ColorIndex = 25
       chWorksheet:range("C24"):FONT:ColorIndex     = 26
       chWorksheet:range("C23"):FONT:ColorIndex     = 27
       chWorksheet:range("C23"):Interior:ColorIndex = 28
       chWorksheet:range("C22"):FONT:ColorIndex     = 29
       chWorksheet:range("C22"):Interior:ColorIndex = 30
       chWorksheet:range("C21"):FONT:ColorIndex     = 31
       chWorksheet:range("C21"):FONT:ColorIndex     = 32
       chWorksheet:range("C20"):Interior:ColorIndex = 33
       chWorksheet:range("C20"):FONT:ColorIndex     = 34
       chWorksheet:range("C19"):Interior:ColorIndex = 41
       chWorksheet:range("C19"):FONT:ColorIndex     = 36
       chWorksheet:range("C18"):Interior:ColorIndex = 38
       chWorksheet:range("C18"):FONT:ColorIndex     = 39
       chWorksheet:range("C17"):Interior:ColorIndex = 40
       chWorksheet:range("C17"):FONT:ColorIndex     = 41
       chWorksheet:range("C16"):Interior:ColorIndex = 42
       chWorksheet:range("C16"):FONT:ColorIndex     = 43
       chWorksheet:range("C15"):Interior:ColorIndex = 44
       chWorksheet:range("C15"):FONT:ColorIndex     = 45
       chWorksheet:range("C14"):Interior:ColorIndex = 46
       chWorksheet:range("C14"):FONT:ColorIndex     = 47
       chWorksheet:range("C13"):Interior:ColorIndex = 48
       chWorksheet:range("C13"):FONT:ColorIndex     = 49
       chWorksheet:range("C12"):Interior:ColorIndex = 50
       chWorksheet:range("C12"):FONT:ColorIndex     = 51
       chWorksheet:range("C11"):FONT:ColorIndex     = 51
       chWorksheet:range("C11"):Interior:ColorIndex = 52
       chWorksheet:range("C10"):FONT:ColorIndex     = 53
       chWorksheet:range("C10"):Interior:ColorIndex = 54
       chWorksheet:range("C09"):FONT:ColorIndex     = 55
       chWorksheet:range("C09"):Interior:ColorIndex = 56
       chWorksheet:range("C08"):FONT:ColorIndex     = 01
       chWorksheet:range("C08"):Interior:ColorIndex = 20
       chWorksheet:range("C07"):FONT:ColorIndex     = 07
       chWorksheet:range("C07"):Interior:ColorIndex = 20
       chWorksheet:range("C06"):FONT:ColorIndex     = 08
       chWorksheet:range("C06"):Interior:ColorIndex = 20
       chWorksheet:range("C05"):FONT:ColorIndex     = 33
       chWorksheet:range("C05"):Interior:ColorIndex = 20
       chWorksheet:range("C04"):FONT:ColorIndex     = 32
       chWorksheet:range("C04"):Interior:ColorIndex = 20
       chWorksheet:range("C03"):FONT:ColorIndex     = 31
       chWorksheet:range("C03"):Interior:ColorIndex = 20
       chWorksheet:range("C02"):FONT:ColorIndex     = 30
       chWorksheet:range("C02"):Interior:ColorIndex = 20
       chWorksheet:range("C01"):FONT:ColorIndex     = 29
       chWorksheet:range("C01"):Interior:ColorIndex = 20.




/* Inclui uma Formula */
ASSIGN chWorksheet:range("A38"):VALUE   = "Total"
       chWorksheet:range("B38"):Formula = "=SUM(B2:B37)".

ASSIGN chWorkSheet:Rows("38"):FONT:ColorIndex     = 49
       chWorkSheet:Rows("38"):Interior:ColorIndex = 50.



/* Criar Um Grafico Dentro da Planilha */ 
chWorksheetRange = chWorksheet:Range("A2:B29"). 
chWorksheet:ChartObjects:Add(300,20,600,300):Activate. /* Parametros:
                                                          1= Coluna
                                                          2= Linha
                                                          3= Tamanho Lateral
                                                          4= Tamanho Horizontal */
chExcelApp:ActiveChart:ChartWizard(chWorksheetRange, 1, 4, 2, 1, 1, TRUE , "Faturamento em U$", "Periodo", "Valores Vendidos"). 
                                              /* Parametros:
                                               1= Tipos do Grafico
                                               2= Modo de Apresenta‡Æo do Grafico (Valores Permitidos 1 a 6)
                                               3= Tratamento no Eixo X (Valores Permitidos 1 ou 2)
                                               4= Inserir Uma Escala no Grafico  e Inverter Eixo X (Valores 0 ou 1)
                                               5= Cria Entradas na Legenda
                                               6=
                                               7= Titulo Top do Grafico
                                               8= Titulo do Eixo Y
                                               9= Titulo do Eixo X */ 
                                                  



/* Cria Uma Planilha de Graficos */ 
chChart=chExcelApp:Charts:Add(). 
ASSIGN chChart:Name = "Grafico U$".


/* Cria OUTRO Grafico dentro da Planilha */ 
chWorksheetRange = chWorksheet:Range("A2:A29;C2:C29"). 
chWorksheet:ChartObjects:Add(300,350,600,300):Activate. /* Parametros:
                                                          1= Coluna
                                                          2= Linha
                                                          3= Tamanho Lateral
                                                          4= Tamanho Horizontal */
chExcelApp:ActiveChart:ChartWizard(chWorksheetRange, 3, 4, 2, 1, 1, TRUE , "Faturamento em R$", "Periodo", "Valores Vendidos"). 
                                              /* Parametros:
                                               1= Tipos do Grafico
                                               2= Modo de Apresenta‡Æo do Grafico (Valores Permitidos 1 a 6)
                                               3= Tratamento no Eixo X (Valores Permitidos 1 ou 2)
                                               4= Inserir Uma Escala no Grafico  e Inverter Eixo X (Valores 0 ou 1)
                                               5= Cria Entradas na Legenda
                                               6=
                                               7= Titulo Top do Grafico
                                               8= Titulo do Eixo Y
                                               9= Titulo do Eixo X */ 





/* Cria Uma Planilha de Graficos */ 
chChart=chExcelApp:Charts:Add(). 
ASSIGN chChart:Name = "Grafico R$".
/*       chChart:Type = 11.  */




/* Congela Celula */
ChWorkbook:Worksheets(1):Activate.
chWorksheet = ChWorkbook:Worksheets(1).
chWorksheet:Range("A2"):SELECT().
chExcelApp:ActiveWindow:freezePanes = TRUE.


/* Salva e Fecha a Planilha */
chExcelApp:DisplayAlerts = FALSE. 
chWorkBook:Save().
if chExcelapp:Version begins "8":U THEN
    chWorkBook:SaveAs(cFileName,39,,,,,,,).
ELSE 
    chWorkBook:SaveAs(cFileName,,,,,,,).
chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.
















































