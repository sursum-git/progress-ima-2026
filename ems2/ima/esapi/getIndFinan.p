DEFINE INPUT  PARAMETER dias        AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER vlInd       AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cErro       AS CHARACTER   NO-UNDO.

//tabela de financiamento fixa
FIND tab-finan NO-LOCK
    WHERE tab-finan.nr-tab-finan = 1 NO-ERROR.

FIND FIRST tab-finan-indice OF tab-finan 
    WHERE tab-finan-indice.tab-dia-fin  >= dias
    NO-LOCK NO-ERROR.

IF AVAIL tab-finan-indice THEN
   ASSIGN vlInd = tab-finan-indice.tab-ind-fin .
ELSE DO:
   ASSIGN vlInd = 0.
   ASSIGN cErro = 'Indice de Financiamento n∆o encontrado'.
END.


   




