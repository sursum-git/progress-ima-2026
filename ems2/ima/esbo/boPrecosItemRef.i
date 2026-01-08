DEFINE TEMP-TABLE ttPreco no-undo
    FIELD itCodigo               LIKE ITEM.it-codigo
    FIELD codRefer               AS CHAR
    FIELD nrContainer            AS INT
    FIELD codOut                 AS CHAR FORMAT 'x(30)'
    FIELD vlPrecoBase            AS DECIMAL
    FIELD vlPrecoAVista          AS DECIMAL
    FIELD vlPreco30Dias          AS DECIMAL
    FIELD vlPreco60Dias          AS DECIMAL
    FIELD vlPreco90Dias          AS DECIMAL
    FIELD vlPrecoPrazo           AS DECIMAL
    FIELD vlPrecoBaseDolar       AS DECIMAL
    FIELD vlPrecoAVistaDolar     AS DECIMAL
    FIELD vlPreco30DiasDolar     AS DECIMAL
    FIELD vlPreco60DiasDolar     AS DECIMAL
    FIELD vlPreco90DiasDolar     AS DECIMAL
    FIELD vlPrecoPrazoDolar      AS DECIMAL
    FIELD tipo                   AS CHAR // pe / pi / outlet
    FIELD codControlePreco       AS INT
    FIELD tbPrecoId              AS INT . 

DEFINE TEMP-TABLE ttIndPrazo no-undo
       FIELD qtDias AS INT
       FIELD indice AS DECIMAL .
