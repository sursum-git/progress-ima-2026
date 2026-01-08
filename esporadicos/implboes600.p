{method/dbotterr.i}
{esbo/boes600.i ttHistPendBook}

DEFINE VARIABLE hBoEs600 AS HANDLE      NO-UNDO.

RUN esbo/boes600.p PERSIST SET hBoEs600.

CREATE ttHistPendBook.
ASSIGN ttHistPendBook.cod_tabela = 'saldo-estoq'
       ttHistPendBook.cod_acao   = 'alteracao_qte' 
       ttHistPendBook.chave      = '505|520078|001' 
       . 
RUN openquerystatic IN hBoes600('main').
run emptyRowObject in hBoEs600.
RUN setRecord IN hBoEs600(INPUT TABLE ttHistPendBook).
RUN createRecord IN hBoEs600.
IF RETURN-VALUE = 'nok' THEN DO:
   RUN getRowErrors IN hBoEs600(OUTPUT TABLE RowErrors).
   OUTPUT TO c:\temp\erros.txt.
   FOR EACH rowErrors:
       EXPORT DELIMITER "|" rowErrors.
   END.

END.



IF VALID-HANDLE(hBoEs600) THEN
   RUN destroy IN hBoEs600.

/**************************************************************************
hist_pend_book_id
dt_hr_registro
cod_tabela
cod_acao
pend_book_id
chave
                                                                           
**************************************************************************/
