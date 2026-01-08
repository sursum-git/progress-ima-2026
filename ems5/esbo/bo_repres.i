DEFINE TEMP-TABLE ttRepres
        FIELD codRepres     AS INT
        FIELD nomeRepres    AS CHAR FORMAT 'x(100)'
        FIELD numPessoa     AS INT
        FIELD codEmitente   AS INT.

DEFINE TEMP-TABLE ttDocs
       FIELD codEstabel         AS CHAR
       FIELD codEmitente        AS INT
       FIELD dtEmissao          AS DATE
       FIELD dtTransacao        AS DATE
       FIELD nroDocto           AS CHAR FORMAT 'x(20)'
       FIELD serieDocto         AS CHAR
       FIELD valorNota          AS DECIMAL
       FIELD dtVencto           AS DATE
       FIELD valorLiquido       AS DECIMAL.

DEFINE TEMP-TABLE ttMeses
        FIELD ano           AS INT
        FIELD mes           AS INT
        FIELD valorNota     AS DECIMAL
        FIELD valorLiquido  AS DECIMAL
        FIELD empresa       AS CHAR.





        
