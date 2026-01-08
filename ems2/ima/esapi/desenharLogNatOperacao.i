DEFINE TEMP-TABLE ttCab NO-UNDO
    FIELD chave      AS CHAR FORMAT 'x(50)'    
    FIELD valor      AS CHAR FORMAT 'x(50)'
    FIELD cLabel     AS CHAR FORMAT 'x(50)'
    INDEX unico IS PRIMARY chave
    .
    
DEFINE TEMP-TABLE ttRegras NO-UNDO    
    FIELD numRegra          AS INT
    FIELD achouEstado       AS CHAR
    .
    
 DEFINE TEMP-TABLE ttRegrasTriang NO-UNDO    
    FIELD numRegraTriang    AS INT
    FIELD numRegra          AS INT
    FIELD natoperacao       AS CHAR
    FIELD estadoTriangOK    AS CHAR
    .   

PROCEDURE inserirTTCab:

    DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pLabel AS CHARACTER   NO-UNDO.
    
    CREATE ttCab.
    ASSIGN ttCab.chave  = pChave
           ttCab.valor  = pValor
           ttCab.cLabel = pLabel
           .

END PROCEDURE.
 
FUNCTION getVlChave RETURNS CHAR(pChave AS CHAR):

    FIND ttCab
        WHERE ttCab.chave = pChave NO-ERROR.
    IF AVAIL ttCab THEN
    DO:
        RETURN ttCab.valor.
        
    END.
    
    RETURN ''.

END FUNCTION.
