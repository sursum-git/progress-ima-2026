
/*Programa:Bo para encriptar e descriptar um texto
Autor: Tadeu Silva 
Data:08/2020
https://knowledgebase.progress.com/articles/Article/P118786
*/


         
DEFINE VARIABLE cTextoEntrada         AS CHARACTER NO-UNDO FORMAT 'x(16000)'.
DEFINE VARIABLE cTextoEntradaCripto   AS RAW NO-UNDO .
DEFINE VARIABLE cAlgoritmo            AS CHARACTER   NO-UNDO FORMAT 'x(50)' INIT "AES_OFB_128" .
DEFINE VARIABLE rBinaryKey            AS RAW       NO-UNDO.
DEFINE VARIABLE cTextoSaida           AS CHARACTER NO-UNDO FORMAT 'x(16000)'.
DEFINE VARIABLE cTextoSaidaRaw        AS RAW NO-UNDO.
         
         
PROCEDURE setTexto:
    DEFINE INPUT  PARAMETER pTexto AS CHARACTER FORMAT 'x(16000)'  NO-UNDO.
    ASSIGN cTextoEntrada = pTexto.
END PROCEDURE.
PROCEDURE settextoCriptografado:
    DEFINE INPUT  PARAMETER  pTextoCripto AS   RaW  NO-UNDO.
    ASSIGN cTextoEntradaCripto = pTextoCripto.
END PROCEDURE.

PROCEDURE setAlgoritmo:
    DEFINE INPUT  PARAMETER pAlgoritmo AS CHARACTER FORMAT 'x(16000)'  NO-UNDO.
    ASSIGN cAlgoritmo = pAlgoritmo.
END PROCEDURE.

PROCEDURE setRawBinaryKey:
    DEFINE INPUT  PARAMETER pBinaryKey AS RAW       NO-UNDO.
    ASSIGN rBinaryKey = pBinaryKey.

END PROCEDURE.

PROCEDURE setTextoChave:
    DEFINE INPUT  PARAMETER pChave AS CHARACTER FORMAT 'x(16)'  NO-UNDO.
    DEFINE VARIABLE iRepet AS INTEGER     NO-UNDO.
    IF LENGTH(pChave) > 16 THEN
       ASSIGN pChave = SUBSTR(pChave,1,16).

    IF LENGTH(pChave) < 16 THEN DO:
       ASSIGN iRepet = 16 - LENGTH(pChave)
              pChave = pChave + FILL(chr(13),iRepet).
    END.
    PUT-STRING(rBinaryKey,1,16) =  pChave.  


END PROCEDURE.

PROCEDURE criptografar:
    ASSIGN 
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = cAlgoritmo  
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = rBinaryKey 
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ? 
    cTextoSaidaRaw  = ENCRYPT(ctextoEntrada)
    cTextoSaida     = BASE64-ENCODE(cTextoSaidaRaw).

END PROCEDURE.


PROCEDURE descriptografar:
    ASSIGN
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = cAlgoritmo
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = rBinaryKey
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?
        cTextoSaida = GET-STRING(DECRYPT (cTextoEntradaCripto),1).

END PROCEDURE.

PROCEDURE getResultado:
    DEFINE OUTPUT PARAMETER cResultado AS CHARACTER   NO-UNDO FORMAT 'x(16000)'.
    ASSIGN cResultado = cTextoSaida.

END PROCEDURE.

PROCEDURE getResultadoRaw:
    DEFINE OUTPUT PARAMETER cResultado AS RAW  NO-UNDO.
    ASSIGN cResultado = cTextoSaidaRaw.

END PROCEDURE.








