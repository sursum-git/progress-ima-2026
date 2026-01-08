//OUTPUT TO c:\temp\teste.txt.
{esp/util.i}
DEFINE TEMP-TABLE ttIbgeCidades NO-UNDO
    FIELD ibge   AS INT
    FIELD cidade AS CHAR FORMAT 'x(30)'
    FIELD lAchou AS LOGICAL
    INDEX ind-cidade cidade
    INDEX ind-ibge ibge 
    INDEX ind-achou lAchou .



DEFINE TEMP-TABLE ttERP LIKE ems2med.cidade
    FIELD codIbge  AS INT                                            
    FIELD nomeIbge AS CHAR FORMAT 'x(20)'
    FIELD logAchou AS LOGICAL
    FIELD cpAchou  AS CHAR
    FIELD logCodIgual AS LOGICAL
    FIELD logNomeIgual AS LOGICAL.


DEFINE TEMP-TABLE ttCodigo
    FIELD codigo AS INT
    FIELD qt     AS INT
    INDEX ind-codigo is PRIMARY  codigo.

INPUT FROM c:\temp\ibge_cidades.csv CONVERT  SOURCE SESSION:CPINTERNAL.
REPEAT:
    CREATE ttIbgeCidades.
    IMPORT DELIMITER ";" ttIbgeCidades.
END.
 
INPUT CLOSE.


FOR EACH ems2med.cidade NO-LOCK.
    CREATE ttErp.
    buffer-copy ems2med.cidade to ttERP.
END.

FOR EACH ttIbgeCidades :
        //DISP ttIbgeCidades WITH 1 COL WIDTH 600.
        FIND FIRST ttERP 
            WHERE ttErp.cdn-munpio-ibge = ttIbgeCidades.ibge
            NO-ERROR.
        IF AVAIL ttErp THEN DO:
           ASSIGN ttErp.logAchou        = YES
                  ttErp.cpAchou         = 'codigo'
                  ttErp.logCodIgual     =  YES
                  ttERp.logNomeIgual    = ttERP.cidade = ttIbgeCidades.cidade 
                  ttErp.nomeIbge        = ttIbgeCidades.cidade
                  ttIbgeCidades.lAchou = YES.
        END.
        ELSE DO:
           FIND FIRST ttERP 
            WHERE ttErp.cidade = ttIbgeCidades.cidade
            NO-ERROR.
            IF AVAIL ttERP THEN DO:
               ASSIGN ttErp.logAchou    = YES
                  ttErp.cpAchou         = 'nome'
                  ttErp.logCodIgual     =  ttERP.cdn-munpio-ibge = ttIbgeCidades.ibge
                  ttErp.codIbge         = ttIbgeCidades.ibge   
                  ttERp.logNomeIgual    =  YES 
                  ttIbgeCidades.lAchou  = YES .

            END.
        END.
END.


FOR EACH ttErp:
    FIND FIRST ttCodigo
        WHERE ttcodigo.codigo = ttErp.cdn-munpio-ibge NO-ERROR.
    IF NOT AVAIL ttcodigo THEN DO:
       CREATE ttCodigo.
       ASSIGN ttcodigo.codigo = ttErp.cdn-munpio-ibge.
    END.                                              
    ASSIGN ttcodigo.qt = ttCodigo.qt + 1 .
END.


OUTPUT TO c:\temp\codigoigual.txt.
FOR EACH ttcodigo WHERE ttcodigo.qt > 1 .
    DISP ttcodigo.codigo ttcodigo.qt .
END.


/*FOR EACH ttERP WHERE 
    ttErp.cdn-munpio-ibge = 3300936
    //ttErp.cidade = 'carapebus'
    .
    DISP ttErp WITH 1 COL 1 DOWN WIDTH 550.
END.*/



















