/* Variaveis para o Excel */
DEFINE VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorkSheet AS COM-HANDLE NO-UNDO.
DEFINE VAR i-Lin       AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".

/* Inicializaá∆o da Planilha */
CREATE "Excel.Application" chExcelApp NO-ERROR.
chExcelApp:SheetsInNewWorkbook = 4. /* Nß PLANILHAS A SEREM CRIADAS */

IF chExcelApp <> ? THEN /* Cria a Planilha */
   ASSIGN chExcelApp:VISIBLE = TRUE.  /* A Planilha n∆o Ficar† Visivel */
          chWorkbook         = chExcelApp:Workbooks:ADD(). /* Cria Planilha */

ASSIGN chWorkSheet        = chExcelapp:Sheets:ITEM(1)
       chWorkSheet:NAME   = "Planilha1".
ASSIGN chWorkSheet        = chExcelapp:Sheets:ITEM(2)
       chWorkSheet:NAME   = "Planilha2".
ASSIGN chWorkSheet        = chExcelapp:Sheets:ITEM(3)
       chWorkSheet:NAME   = "Planilha3".
ASSIGN chWorkSheet        = chExcelapp:Sheets:ITEM(4)
       chWorkSheet:NAME   = "Planilha4".

/* Seleciona a Primeira Planilha */
ChWorkbook:Worksheets(1):activate.
chWorksheet = ChWorkbook:Worksheets(1).
chWorksheet:range("A1"):VALUE = "Conteudo A1".
chWorksheet:range("A2"):VALUE = 100.
chWorksheet:range("A3"):VALUE = 200.
chWorksheet:range("A4"):VALUE = 300.
chWorksheet:range("B2"):VALUE = "A".
chWorksheet:range("B3"):VALUE = "B".
chWorksheet:range("B4"):VALUE = "C".

chWorksheet:range("A5"):FORMULA = "=SUMIF(R2C2:R4C2," + '"A"' + ",R2C1:R4C1)".
chWorksheet:range("A6"):VALUE = "=SUMIF(B2:B4,""B"",A2:A4)".
chWorkSheet:range("A2:B4"):LOCKED = FALSE.
/*chWorkSheet:Protect("",True,True,True).*/

chWorksheet:range("C1:C5"):SELECT().
chExcelApp:Selection:Validation.
  .Delete
  .ADD TYPE(xlValidateWholeNumber, AlertStyle, xlValidAlertStop, Operator, xlBetween, Formula1, 0, Formula2, 5)
  .IgnoreBlank    = TRUE
  .InCellDropdown = TRUE
  .InputTitle     = "Prazos"
  .ErrorTitle     = "Prazo Incorreto!"
  .InputMessage   = "Prazos: 0 = ∑ vista, 1 a 5 = Parcelado."
  .ErrorMessage   = "Prazo deve estar entre 0 e 5."
  .ShowInput      = TRUE
  .ShowError      = TRUE.
/*
 With Selection.Validation
        .Delete
        .Add Type:=xlValidateWholeNumber, AlertStyle:=xlValidAlertStop, _
        Operator:=xlBetween, Formula1:="0", Formula2:="5"
        .IgnoreBlank = True
        .InCellDropdown = True
        .InputTitle = "Prazos"
        .ErrorTitle = "Prazo incorreto!"
        .InputMessage = "Prazos: 0 = ∑ vista, 1 a 5 = Parcelado."
        .ErrorMessage = "Prazo deve estar entre 0 e 5."
        .ShowInput = True
        .ShowError = True
*/

MESSAGE "Gravei 1"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Seleciona a Segunda Planilha */
ChWorkbook:Worksheets(2):activate.
chWorksheet = ChWorkbook:Worksheets(2).
chWorksheet:range("A1"):VALUE = "Conteudo A1".
MESSAGE "Gravei 2"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Seleciona a Terceira Planilha */
ChWorkbook:Worksheets(3):activate.
chWorksheet = ChWorkbook:Worksheets(3).
chWorksheet:range("A1"):VALUE = "Conteudo A1".
MESSAGE "Gravei 3"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Seleciona a Quarta Planilha */
ChWorkbook:Worksheets(4):activate.
chWorksheet = ChWorkbook:Worksheets(4).
chWorksheet:range("A1"):VALUE = "Conteudo A1".
MESSAGE "Gravei 4"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

MESSAGE "Criei - Irei sair sem salvar"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* Fecha Planilha e Excel */
chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chWorkSheet.
