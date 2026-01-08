DEFINE INPUT  PARAMETER pRowid  AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER rowidNF AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
FIND ped-venda NO-LOCK
    WHERE ROWID(ped-venda) = pRowid
    NO-ERROR.
IF AVAIL ped-venda THEN DO:
   FIND LAST nota-fiscal NO-LOCK
       WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
       AND   nota-fiscal.nr-pedcli  = ped-venda.nr-pedcli
       AND   nota-fiscal.dt-cancela = ?
       NO-ERROR.                       
   IF AVAIL nota-fiscal THEN DO:
      ASSIGN rowidNF = ROWID(nota-fiscal).
   END.
   ELSE DO:
      ASSIGN cErro = "Nota Fiscal N∆o encontrada para o Pedido de venda".
      RETURN 'nok'.
   END.
END.
ELSE DO:
    ASSIGN cErro = "Pedido de venda n∆o encontrado com o Rowid passado".
    RETURN 'nok'.
END.


