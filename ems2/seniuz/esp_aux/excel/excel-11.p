DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF var chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR cFileName   AS CHAR.
DEF VAR c-lixo      AS CHAR.
DEF VAR i-lin       AS INT.
DEF VAR i-ct        AS INT.

ASSIGN cFileName = "C:\TEMP\TESTE.XLS".
/* OS-DELETE VALUE(cFileName). */

RUN pi-abre-excel.
IF chExcelApp = ? THEN DO:
   MESSAGE "O Aplicativo EXCEL n∆o encontrado. Favor instalar."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN.
END.

/* Seleciona a Primeira Planilha */
ASSIGN i-lin = 3.
ChWorkbook:Worksheets(1):activate.
chWorksheet = ChWorkbook:Worksheets(1).
chworksheet:NAME="FCL". 
RUN pi-imp-cabec (INPUT 1).
RUN pi-dados-excel.
ASSIGN chWorksheet:range("A" + STRING(i-lin)):VALUE   = "TOTAL GERAL....."
       chWorksheet:range("B" + STRING(i-lin)):Formula = "=SUM(B3:B" + STRING(i-lin - 1) + ")"
       chWorksheet:range("C" + STRING(i-lin)):Formula = "=SUM(C3:C" + STRING(i-lin - 1) + ")"
       chWorksheet:range("D" + STRING(i-lin)):Formula = "=SUM(D3:D" + STRING(i-lin - 1) + ")"
       chWorksheet:range("E" + STRING(i-lin)):Formula = "=SUM(E3:E" + STRING(i-lin - 1) + ")"
       chWorksheet:range("F" + STRING(i-lin)):Formula = "=SUM(F3:F" + STRING(i-lin - 1) + ")"
       chWorksheet:range("G" + STRING(i-lin)):Formula = "=SUM(G3:G" + STRING(i-lin - 1) + ")"
       chWorksheet:range("H" + STRING(i-lin)):Formula = "=SUM(H3:H" + STRING(i-lin - 1) + ")"
       chWorksheet:range("I" + STRING(i-lin)):Formula = "=SUM(I3:I" + STRING(i-lin - 1) + ")"
       chWorksheet:range("J" + STRING(i-lin)):Formula = "=SUM(J3:J" + STRING(i-lin - 1) + ")"
       chWorksheet:range("K" + STRING(i-lin)):Formula = "=SUM(K3:K" + STRING(i-lin - 1) + ")"
       chWorksheet:range("L" + STRING(i-lin)):Formula = "=SUM(L3:L" + STRING(i-lin - 1) + ")"
       chWorksheet:range("M" + STRING(i-lin)):Formula = "=SUM(M3:M" + STRING(i-lin - 1) + ")"
       chWorksheet:range("N" + STRING(i-lin)):Formula = "=SUM(N3:N" + STRING(i-lin - 1) + ")".

chWorkSheet:Range("B1:N1"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Impact"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 6
       chExcelApp:SELECTION:FONT:ColorIndex = 10
       chExcelApp:SELECTION:Style = "Currency".


chWorksheet:range("B" + STRING(i-lin) + ":N" + STRING(i-lin)):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Arial"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 10
       chExcelApp:SELECTION:FONT:ColorIndex = 15
       chExcelApp:SELECTION:Style = "Currency".

ASSIGN chWorksheet:COLUMNS("A"):FONT:ColorIndex       = 45
       chWorksheet:COLUMNS("A:A"):Interior:ColorIndex = 8
       chWorksheet:COLUMNS("A"):FONT:NAME             = "Courrier New"
       chWorkSheet:COLUMNS("A"):ColumnWidth           = 22
       chWorksheet:COLUMNS("A"):FONT:Bold             = TRUE
       chWorksheet:COLUMNS("A"):FONT:Italic           = TRUE.

ASSIGN chWorksheet:COLUMNS("B:N"):FONT:NAME   = "Bodoni MT Black"
       chWorkSheet:COLUMNS("B:N"):ColumnWidth = 16
       chWorkSheet:Range("B:N"):FONT:SIZE     = 16
       chworksheet:range("B:N"):NumberFormat  = "###.###.##0,00"
       Chworksheet:range("B:N"):HorizontalAlignment = 4.

chWorksheet:range("B2:B" + STRING(i-lin - 1)):SELECT().
ASSIGN chExcelApp:SELECTION:Interior:ColorIndex = 9
       chExcelApp:SELECTION:FONT:ColorIndex = 15.

chWorksheet:range("C2:C" + STRING(i-lin - 1)):SELECT().
ASSIGN chExcelApp:SELECTION:Interior:ColorIndex = 11
       chExcelApp:SELECTION:FONT:ColorIndex = 15.

chWorksheet:range("D2:D" + STRING(i-lin - 1)):SELECT().
ASSIGN chExcelApp:SELECTION:Interior:ColorIndex = 12
       chExcelApp:SELECTION:FONT:ColorIndex = 16.

chWorksheet:range("E2:E" + STRING(i-lin - 1)):SELECT().
ASSIGN chExcelApp:SELECTION:Interior:ColorIndex = 13
       chExcelApp:SELECTION:FONT:ColorIndex = 17.

/* Seleciona a Segunda Planilha */
ASSIGN i-lin = 3.
ChWorkbook:Worksheets(2):Activate.
chWorksheet = ChWorkbook:Worksheets(2).
chworksheet:NAME="VBCL". 
RUN pi-imp-cabec (INPUT 2).
RUN pi-dados-excel.
ASSIGN chWorksheet:range("A" + STRING(i-lin)):VALUE   = "TOTAL GERAL....."
       chWorksheet:range("B" + STRING(i-lin)):Formula = "=SUM(B3:B" + STRING(i-lin - 1) + ")"
       chWorksheet:range("C" + STRING(i-lin)):Formula = "=SUM(C3:C" + STRING(i-lin - 1) + ")"
       chWorksheet:range("D" + STRING(i-lin)):Formula = "=SUM(D3:D" + STRING(i-lin - 1) + ")"
       chWorksheet:range("E" + STRING(i-lin)):Formula = "=SUM(E3:E" + STRING(i-lin - 1) + ")"
       chWorksheet:range("F" + STRING(i-lin)):Formula = "=SUM(F3:F" + STRING(i-lin - 1) + ")"
       chWorksheet:range("G" + STRING(i-lin)):Formula = "=SUM(G3:G" + STRING(i-lin - 1) + ")"
       chWorksheet:range("H" + STRING(i-lin)):Formula = "=SUM(H3:H" + STRING(i-lin - 1) + ")"
       chWorksheet:range("I" + STRING(i-lin)):Formula = "=SUM(I3:I" + STRING(i-lin - 1) + ")"
       chWorksheet:range("J" + STRING(i-lin)):Formula = "=SUM(J3:J" + STRING(i-lin - 1) + ")"
       chWorksheet:range("K" + STRING(i-lin)):Formula = "=SUM(K3:K" + STRING(i-lin - 1) + ")"
       chWorksheet:range("L" + STRING(i-lin)):Formula = "=SUM(L3:L" + STRING(i-lin - 1) + ")"
       chWorksheet:range("M" + STRING(i-lin)):Formula = "=SUM(M3:M" + STRING(i-lin - 1) + ")"
       chWorksheet:range("N" + STRING(i-lin)):Formula = "=SUM(N3:N" + STRING(i-lin - 1) + ")".

chWorkSheet:Range("B1:N1"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Berlin Sans FB"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 7
       chExcelApp:SELECTION:FONT:ColorIndex = 11
       chExcelApp:SELECTION:Style = "Currency".


chWorksheet:range("B" + STRING(i-lin) + ":N" + STRING(i-lin)):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Century Gothic"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 12
       chExcelApp:SELECTION:FONT:ColorIndex = 17
       chExcelApp:SELECTION:Style = "Currency".

ASSIGN chWorksheet:COLUMNS("A"):FONT:ColorIndex       = 35
       chWorksheet:COLUMNS("A:A"):Interior:ColorIndex = 7
       chWorksheet:COLUMNS("A"):FONT:NAME             = "Constantia"
       chWorkSheet:COLUMNS("A"):ColumnWidth           = 22
       chWorksheet:COLUMNS("A"):FONT:Bold             = TRUE
       chWorksheet:COLUMNS("A"):FONT:Italic           = TRUE.

ASSIGN chWorksheet:COLUMNS("B:N"):FONT:NAME   = "Arial"
       chWorkSheet:COLUMNS("B:N"):ColumnWidth = 16
       chWorkSheet:Range("B:N"):FONT:SIZE     = 16
       chworksheet:range("B:N"):NumberFormat  = "###.###.##0,00"
       Chworksheet:range("B:N"):HorizontalAlignment = 4.

/* Seleciona a Terceira Planilha */
ASSIGN i-lin = 3.
ChWorkbook:Worksheets(3):Activate.
chWorksheet = ChWorkbook:Worksheets(3).
chworksheet:NAME="VCL". 
RUN pi-imp-cabec (INPUT 3).
RUN pi-dados-excel.
ASSIGN chWorksheet:range("A" + STRING(i-lin)):VALUE   = "TOTAL GERAL....."
       chWorksheet:range("B" + STRING(i-lin)):Formula = "=SUM(B3:B" + STRING(i-lin - 1) + ")"
       chWorksheet:range("C" + STRING(i-lin)):Formula = "=SUM(C3:C" + STRING(i-lin - 1) + ")"
       chWorksheet:range("D" + STRING(i-lin)):Formula = "=SUM(D3:D" + STRING(i-lin - 1) + ")"
       chWorksheet:range("E" + STRING(i-lin)):Formula = "=SUM(E3:E" + STRING(i-lin - 1) + ")"
       chWorksheet:range("F" + STRING(i-lin)):Formula = "=SUM(F3:F" + STRING(i-lin - 1) + ")"
       chWorksheet:range("G" + STRING(i-lin)):Formula = "=SUM(G3:G" + STRING(i-lin - 1) + ")"
       chWorksheet:range("H" + STRING(i-lin)):Formula = "=SUM(H3:H" + STRING(i-lin - 1) + ")"
       chWorksheet:range("I" + STRING(i-lin)):Formula = "=SUM(I3:I" + STRING(i-lin - 1) + ")"
       chWorksheet:range("J" + STRING(i-lin)):Formula = "=SUM(J3:J" + STRING(i-lin - 1) + ")"
       chWorksheet:range("K" + STRING(i-lin)):Formula = "=SUM(K3:K" + STRING(i-lin - 1) + ")"
       chWorksheet:range("L" + STRING(i-lin)):Formula = "=SUM(L3:L" + STRING(i-lin - 1) + ")"
       chWorksheet:range("M" + STRING(i-lin)):Formula = "=SUM(M3:M" + STRING(i-lin - 1) + ")"
       chWorksheet:range("N" + STRING(i-lin)):Formula = "=SUM(N3:N" + STRING(i-lin - 1) + ")".

chWorkSheet:Range("B1:N1"):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Bell MT"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 3
       chExcelApp:SELECTION:FONT:ColorIndex = 23
       chExcelApp:SELECTION:Style = "Currency".


chWorksheet:range("B" + STRING(i-lin) + ":N" + STRING(i-lin)):SELECT().
ASSIGN chExcelApp:SELECTION:FONT:NAME = "Bodoni MT"
       chExcelApp:SELECTION:FONT:SIZE = 22
       chExcelApp:SELECTION:Interior:ColorIndex = 13
       chExcelApp:SELECTION:FONT:ColorIndex = 18
       chExcelApp:SELECTION:Style = "Currency".

ASSIGN chWorksheet:COLUMNS("A"):FONT:ColorIndex       = 32
       chWorksheet:COLUMNS("A:A"):Interior:ColorIndex = 19
       chWorksheet:COLUMNS("A"):FONT:NAME             = "Californian FB"
       chWorkSheet:COLUMNS("A"):ColumnWidth           = 22
       chWorksheet:COLUMNS("A"):FONT:Bold             = TRUE
       chWorksheet:COLUMNS("A"):FONT:Italic           = TRUE.

ASSIGN chWorksheet:COLUMNS("B:N"):FONT:NAME   = "Consolas"
       chWorkSheet:COLUMNS("B:N"):ColumnWidth = 16
       chWorkSheet:Range("B:N"):FONT:SIZE     = 16
       chworksheet:range("B:N"):NumberFormat  = "###.###.##0,00"
       Chworksheet:range("B:N"):HorizontalAlignment = 4.


/* Posiciona o Foco no Inicio da Planilha */
ChWorkbook:Worksheets(1):Activate.
chWorksheet = ChWorkbook:Worksheets(1).
chWorksheet:Range("A1"):SELECT.
chExcelApp:ActiveWindow:FreezePanes = FALSE.

/* Salva e Fecha a Planilha */
/* chWorkBook:SaveAs(cFileName,,,,,,,). */
IF chExcelApp:Version BEGINS "8":U THEN 
   chWorkBook:SaveAs(cFileName,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
ELSE 
   chWorkBook:SaveAs(cFileName,,,,,,,).  /* Salva na Vers∆o da planilha da ESTAÄ«O */

chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.



PROCEDURE pi-abre-excel. /* Cria Uma Nova Planilha Com Mais 2 Planilhas */
    CREATE "Excel.Application" chExcelApp NO-ERROR. 
    chExcelApp:SheetsInNewWorkbook = 5. /* Nß PLANILHAS A SEREM CRIADAS */

    /*
  Application.SheetsInNewWorkbook = 4
  
chExcelAppl:SheetsInNewWorkbook = 9.
/* Then to access and populate */
DO i = 1 TO 9:
   chWorksheet = chExcelAppl:Sheets:Item(i).
   chWorksheet:Name = "Data Sheet" + STRING(i).
   /* Write data */
END.  
  
  

    */

    IF chExcelApp <> ? THEN DO:
       ASSIGN cFileName = "C:\TEMP\TESTE.XLS".
    
       /* Cria a Planilha */
       ASSIGN chExcelApp:VISIBLE     = FALSE  /* A Planilha N«O Ficar† Visivel */
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
       ASSIGN chExcelApp:VISIBLE     = TRUE 
              chWorkbook  = chExcelApp:Workbooks:OPEN(cFileName).
    
       /* Incluir Uma Nova Planilha */
       ChWorksheet = chWorkbook:Worksheets(1).
       chWorkbook:Worksheets:add(,chWorksheet).
    END.
END.




PROCEDURE pi-imp-cabec.

    DEF INPUT PARAMETER p-planilha AS INT.
    CASE p-planilha:
        WHEN 1 THEN
            ASSIGN chworksheet:range("A1"):VALUE = "FµBIO COELHO LANZA".
        WHEN 2 THEN
            ASSIGN chworksheet:range("A1"):VALUE = "VICENTINA BIASUTTI CLEMENTE LANZA".
        WHEN 3 THEN
            ASSIGN chworksheet:range("A1"):VALUE = "VITOR CLEMENTE LANZA".
    END.
    ChWorkSheet:range("A1:N1"):SELECT().
    ChWorksheet:range("A1:N1"):Merge.
    ASSIGN Chworksheet:Range("A1:N1"):HorizontalAlignment =  3.


    ASSIGN chworksheet:range("B2"):VALUE = "Coluna B"
           chworksheet:range("C2"):VALUE = "Coluna C"
           chworksheet:range("D2"):VALUE = "Coluna D"   
           chworksheet:range("E2"):VALUE = "Coluna E"
           chworksheet:range("F2"):VALUE = "Coluna F"
           chworksheet:range("G2"):VALUE = "Coluna G"
           chworksheet:range("H2"):VALUE = "Coluna H"
           chworksheet:range("I2"):VALUE = "Coluna I"
           chworksheet:range("J2"):VALUE = "Coluna J"
           chworksheet:range("K2"):VALUE = "Coluna K" 
           chworksheet:range("L2"):VALUE = "Coluna L" 
           chworksheet:range("M2"):VALUE = "Coluna M"
           chworksheet:range("N2"):VALUE = "Coluna N".

END.


PROCEDURE pi-dados-excel.

    DO  i-ct = 1 TO 20:
        ASSIGN chworksheet:range("B" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("C" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("D" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("E" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("F" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("G" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("H" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("I" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("J" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("K" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("L" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("M" + STRING(i-lin)):VALUE = i-ct
               chworksheet:range("N" + STRING(i-lin)):VALUE = i-ct
               i-lin = i-lin + 1.
    END.
END.
