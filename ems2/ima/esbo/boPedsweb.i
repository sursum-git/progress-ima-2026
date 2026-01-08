PROCEDURE getDeParaMoedaPedWeb:
    DEFINE INPUT  PARAMETER pMoeda  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iMoeda AS INTEGER     NO-UNDO.
    CASE pMoeda:
        WHEN 'real' THEN
          ASSIGN iMoeda = 0.
        WHEN 'dolar' THEN
          ASSIGN iMoeda = 3.
    END CASE.              

END PROCEDURE.
