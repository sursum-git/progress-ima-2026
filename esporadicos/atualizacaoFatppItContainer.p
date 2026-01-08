DEFINE TEMP-TABLE ttContainerPend NO-UNDO
    FIELD nrContainer    AS INT
    FIELD itCodigo       AS CHAR
    FIELD codRefer       AS CHAR  
    FIELD qtFaturada     AS DECIMAL FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD qtDevolvida    AS DECIMAL FORMAT '->>>,>>>,>>>,>>9.99'
    FIELD logAtu         AS LOGICAL
    FIELD existContainer AS  LOGICAL
    INDEX ind-unico    IS PRIMARY  nrContainer itCodigo codRefer .

FOR EACH fats_04 NO-LOCK
    WHERE fats_04.num_origem > 0.
    FIND ttContainerPend
        WHERE ttContainerPend.nrContainer = fats_04.num_origem 
        AND   ttContainerPend.itCodigo    = fats_04.it_codigo
        AND   ttContainerPend.codRefer    = fats_04.cod_refer
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttContainerPend THEN DO:
       CREATE ttContainerPend.
       ASSIGN ttContainerPend.nrContainer = fats_04.num_origem
              ttContainerPend.itCodigo    = fats_04.it_codigo
              ttContainerPend.codRefer    = fats_04.cod_refer.

    END.
    ASSIGN ttContainerPend.qtFaturada  = ttContainerPend.qtFaturada   + fats_04.qt_total 
           ttContainerPend.qtDevolvida = ttContainerPend.qtDevolvida  + fats_04.qt_dev_total
           .

END.


FOR EACH ttContainerPend.
    FIND pp-it-container EXCLUSIVE-LOCK
        WHERE pp-it-container.nr-container = ttContainerPend.nrContainer
        AND   pp-it-container.it-codigo    = ttContainerPend.itCodigo   
        AND   pp-it-container.cod-refer    = ttContainerPend.codRefer NO-ERROR.
    IF AVAIL pp-it-container THEN DO:
       ASSIGN pp-it-container.qt_faturada  = ttContainerPend.qtFaturada
              pp-it-container.qt_devolvida = ttContainerPend.qtDevolvida
             . 
       RELEASE pp-it-container.
       ASSIGN ttContainerPend.logAtu = YES.
    END.
END.

OUTPUT TO c:\temp\fat_perdido_container.csv.
FOR EACH ttContainerPend
    //WHERE ttContainerPend.logAtu = NO
    .
    FIND pp-container NO-LOCK
        WHERE pp-container.nr-container = ttContainerPend.nrContainer NO-ERROR.
    ASSIGN ttContainerPend.existContainer = AVAIL pp-container.
    EXPORT DELIMITER ";" ttContainerPend .
END.
OUTPUT CLOSE.
