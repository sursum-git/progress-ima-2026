DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF var chWorksheet AS COM-HANDLE NO-UNDO.
DEF VAR cFileName   AS CHAR.
DEF VAR i-lin       AS INT INITIAL 2.
DEF VAR i-ct        AS INT.


CREATE "Excel.Application" chExcelApp.
MESSAGE chExcelApp
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
