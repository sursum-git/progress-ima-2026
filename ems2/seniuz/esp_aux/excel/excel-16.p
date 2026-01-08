DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF var chWorksheet AS COM-HANDLE NO-UNDO.
DEF VAR i-Lin       AS INT.
DEF VAR c-comando   AS CHAR.

DEF TEMP-TABLE tt-venda-loja
    FIELD empresa    AS INT
    FIELD estab      AS INT
    FIELD matric     AS INT
    FIELD nome       AS CHAR
    FIELD setor      AS CHAR
    FIELD dt-admis   AS CHAR
    FIELD limite     AS DEC
    FIELD sld-disp   AS DEC
    FIELD so-pode    AS CHAR
    FIELD vlr-compra AS DEC
    FIELD prazo      AS INT
    FIELD parcela    AS DEC
    FIELD a-vista    AS DEC
    FIELD parc-unica AS DEC
    FIELD parcelado  AS DEC
    INDEX ch-venda empresa
                   matric.


/* Inicializa‡Æo da Planilha */
CREATE "Excel.Application" chExcelApp NO-ERROR.
IF chExcelApp <> ? THEN /* Cria a Planilha */
   ASSIGN chExcelApp:SheetsInNewWorkbook = 1 /* N§ PLANILHAS A SEREM CRIADAS */
          chExcelApp:VISIBLE = FALSE  /* A Planilha nÆo Ficar  Visivel */
          chWorkbook         = chExcelApp:Workbooks:OPEN("T:\janete\Lojinha\venda_loja_funcionario.xls")
          chWorksheet        = chExcelapp:Sheets:ITEM(1).
ELSE DO:
   MESSAGE "O Aplicativo EXCEL nÆo foi encontrado. NÆo foi poss¡vel executar o programa."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN.
END.

/* Atualiza a Temp-Table */
chWorkbook:Worksheets(1):activate.
ASSIGN i-Lin = 2.
REPEAT:
   IF chWorksheet:range("A" + STRING(i-Lin)):VALUE = ? THEN LEAVE.

   CREATE tt-venda-loja.
   ASSIGN tt-venda-loja.empresa    = IF chWorksheet:range("A" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("A" + STRING(i-Lin)):VALUE  
          tt-venda-loja.estab      = IF chWorksheet:range("B" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("B" + STRING(i-Lin)):VALUE  
          tt-venda-loja.matric     = IF chWorksheet:range("C" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("C" + STRING(i-Lin)):VALUE  
          tt-venda-loja.nome       = IF chWorksheet:range("D" + STRING(i-Lin)):VALUE = ? THEN "" ELSE chWorksheet:range("D" + STRING(i-Lin)):VALUE
          tt-venda-loja.setor      = IF chWorksheet:range("E" + STRING(i-Lin)):VALUE = ? THEN "" ELSE chWorksheet:range("E" + STRING(i-Lin)):VALUE
          tt-venda-loja.dt-admis   = IF chWorksheet:range("F" + STRING(i-Lin)):VALUE = ? THEN "" ELSE chWorksheet:range("F" + STRING(i-Lin)):VALUE  
          tt-venda-loja.limite     = IF chWorksheet:range("G" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("G" + STRING(i-Lin)):VALUE  
          tt-venda-loja.sld-disp   = IF chWorksheet:range("H" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("H" + STRING(i-Lin)):VALUE  
          tt-venda-loja.so-pode    = IF chWorksheet:range("I" + STRING(i-Lin)):VALUE = ? THEN "" ELSE chWorksheet:range("I" + STRING(i-Lin)):VALUE  
          tt-venda-loja.vlr-compra = IF chWorksheet:range("J" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("J" + STRING(i-Lin)):VALUE  
          tt-venda-loja.prazo      = IF chWorksheet:range("K" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("K" + STRING(i-Lin)):VALUE  
          tt-venda-loja.parcela    = IF chWorksheet:range("L" + STRING(i-Lin)):VALUE = ? THEN 0 ELSE  chWorksheet:range("L" + STRING(i-Lin)):VALUE. 
   ASSIGN i-Lin = i-Lin + 1.
END.

/* Fecha Planilha */
chExcelApp:DisplayAlerts = FALSE. 
chWorkBook:CLOSE().

FOR EACH tt-venda-loja.
    DISP tt-venda-loja.vlr-compra FORMAT ">>>>>>>9.9999999999999"
         tt-venda-loja.vlr-compra > 0
         DEC(STRING(tt-venda-loja.vlr-compra)) > 0.
    DISP tt-venda-loja 
         WITH SIDE-LABELS 1 COLUMN.
END.

