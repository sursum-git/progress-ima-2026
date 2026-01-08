/*DEFINE TEMP-TABLE ttErp
    FIELD itCodigo AS CHAR
    FIELD codRefer AS CHAR
    FIELD nota     AS CHAR
    FIELD qtSaldo  AS DECIMAL.
  */

OUTPUT TO c:\temp\saldo_erp_v2.csv.
FOR EACH saldo-terc NO-LOCK
    WHERE saldo-terc.cod-estabel = '505'.
    EXPORT DELIMITER ";" 
        saldo-terc.it-codigo
        saldo-terc.cod-refer
        saldo-terc.nro-docto
        saldo-terc.quantidade * -1.

END.
