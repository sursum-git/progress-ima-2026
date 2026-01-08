DEFINE TEMP-TABLE ttSemana 
    FIELD ordem      AS INT
    FIELD diaInicial AS DATE
    FIELD diaFinal   AS DATE
    FIELD vlFinal    AS DECIMAL.

DEFINE TEMP-TABLE ttDia 
    FIELD ordem AS INT
    FIELD data  AS DATE
    FIELD vlFinal AS DECIMAL
    INDEX ind-data data.
