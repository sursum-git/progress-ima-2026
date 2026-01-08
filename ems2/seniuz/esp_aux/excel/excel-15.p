/* Variaveis para o Excel */
DEFINE VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorkSheet AS COM-HANDLE NO-UNDO.
DEFINE VAR i-Lin       AS INTEGER INIT 1.

/* Inicializaá∆o da Planilha */
CREATE "Excel.Application" chExcelApp NO-ERROR.
chExcelApp:SheetsInNewWorkbook = 1. /* Nß PLANILHAS A SEREM CRIADAS */

IF chExcelApp <> ? THEN /* Cria a Planilha */
   ASSIGN chExcelApp:VISIBLE = TRUE.  /* A Planilha Ficar† Visivel */
          chWorkbook         = chExcelApp:Workbooks:ADD(). /* Cria Planilha */

ASSIGN chWorkSheet        = chExcelapp:Sheets:ITEM(1)
       chWorkSheet:NAME   = "Planilha1".

/* Seleciona a Primeira Planilha */
ChWorkbook:Worksheets(1):activate.
chWorksheet = ChWorkbook:Worksheets(1).
chWorksheet:range("A1"):VALUE = "Nome".
chWorksheet:range("A2"):VALUE = "Admiss∆o".

FOR EACH funcionario WHERE funcionario.dat_desligto_func = ? NO-LOCK.
    FIND rh_pessoa_fisic OF funcionario NO-LOCK.
    chWorksheet:range("A" + STRING(i-Lin)):VALUE = rh_pessoa_fisic.nom_pessoa_fisic.        
    chWorksheet:range("B" + STRING(i-Lin)):VALUE = STRING(funcionario.dat_admis_func,"99/99/9999").
    ASSIGN i-Lin = i-Lin + 1.
END.

MESSAGE "Criei - Irei sair sem salvar"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* Fecha Planilha e Excel */
chWorkBook:CLOSE().
chExcelApp:QUIT().
RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chWorkSheet.
