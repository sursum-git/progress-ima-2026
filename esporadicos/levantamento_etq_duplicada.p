DEFINE TEMP-TABLE ttetq NO-UNDO
    FIELD numEtq        AS INT
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD nrContainer   AS INT
    FIELD numRolo       AS INT
    FIELD quantidade    AS DECIMAL
    INDEX primario IS PRIMARY itCodigo CodRefer nrContainer numRolo
    INDEX unico IS UNIQUE numEtq.




DEFINE TEMP-TABLE ttRolo NO-UNDO
    FIELD itCodigo       AS CHAR
    FIELD codRefer       AS CHAR
    FIELD nrContainer    AS INT
    FIELD numRolo        AS INT
    FIELD qt             AS INT
    INDEX primario IS PRIMARY IS UNIQUE itCodigo CodRefer nrContainer numRolo .




FOR EACH ob-etiqueta NO-LOCK
    WHERE ob-etiqueta.situacao = 3
    AND ob-etiqueta.cod-estabel = '505':
    CREATE ttEtq.
    ASSIGN ttEtq.numEtq      = ob-etiqueta.num-etiqueta
           ttEtq.itCodigo    = ob-etiqueta.it-codigo
           ttEtq.codRefer    = ob-etiqueta.cod-refer
           ttEtq.nrContainer = ob-etiqueta.nr-container
           ttEtq.numRolo     = ob-etiqueta.num-rolo
           ttEtq.quantidade  = ob-etiqueta.quantidade
           .

    FIND ttRolo NO-LOCK
        WHERE ttRolo.itCodigo     = ob-etiqueta.it-codigo     
        AND   ttRolo.codRefer     = ob-etiqueta.cod-refer     
        AND   ttRolo.nrContainer  = ob-etiqueta.nr-container  
        AND   ttRolo.numRolo      = ob-etiqueta.num-rolo   
        NO-ERROR.
    IF NOT AVAIL ttRolo  THEN DO:
       CREATE ttRolo.
       assign ttRolo.itCodigo     = ob-etiqueta.it-codigo     
              ttRolo.codRefer     = ob-etiqueta.cod-refer    
              ttRolo.nrContainer  = ob-etiqueta.nr-container 
              ttRolo.numRolo      = ob-etiqueta.num-rolo.
    END.
    ASSIGN ttRolo.qt = ttRolo.qt + 1 .

END.

OUTPUT TO c:\temp\etqRepetidas.csv.
FOR EACH ttRolo
    WHERE ttRolo.qt > 1:
    FOR EACH ttEtq
        WHERE ttEtq.itCodigo      = ttRolo.itCodigo   
        AND   ttEtq.codRefer      = ttRolo.codRefer   
        AND   ttEtq.nrContainer   = ttRolo.nrContainer
        AND   ttEtq.numRolo       = ttRolo.numRolo .
        EXPORT DELIMITER ";" ttEtq.
    END.
END.         
OUTPUT CLOSE.


OUTPUT TO c:\temp\etqRepetidas2.csv.
FOR EACH ttRolo
    WHERE ttRolo.qt > 1:
    EXPORT DELIMITER ";" ttRolo.
END.         
OUTPUT CLOSE.
