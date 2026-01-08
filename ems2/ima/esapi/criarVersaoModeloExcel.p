/***************************************************************************
programa:esapi/criarVersaoModeloExcel.p
objetivo: Devido ao parametro /t do excel n∆o funcionar mais em 2025
estamos criando est† api que salvar† uma copia do excel modelo para
simular o que o /t fazia
*****************************************************************************/
DEFINE INPUT  PARAMETER pModelo     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER novaVersao  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE chExcel     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook  AS COM-HANDLE NO-UNDO.

ASSIGN novaVersao = SESSION:TEMP-DIRECTORY 
                    + STRING(RANDOM(1,99999)) 
                    + STRING(TIME) + '.xlsx'.  


ASSIGN pModelo = SEARCH(pModelo).
IF pModelo = ? THEN DO:
  RETURN 'nok'.    
END.


CREATE "excel.application" chExcel.
chworkbook = chExcel:Workbooks:Open(search(pModelo)).
//chExcel:visible = FALSE.
chExcel:DisplayAlerts = FALSE.
/*ChExcel:ActiveSheet:range('d5'):SELECT.
ChExcel:selection:QueryTable:Refresh.

ChExcel:ActiveSheet:range('c12'):SELECT.
ChExcel:selection:QueryTable:Refresh.*/

chworkbook:SaveAs(novaVersao,61,,,,,,,TRUE).
//chExcel:ActiveSheet
//chWorkbook:SaveAs(xSaveAsName,-4143 /DefaultFormat/,/Password/,/WriteResPassword/,/ReadOnlyRecommended/,/CreateBackup/,/AccessMode/,/ConflictResolution/,/AddToMru/,/TextCodepage/,/TextVisualLayout/,TRUE /Local/).

chWorkbook:CLOSE.
//chExcel:ActiveSheet:close. 


