/*
programa:esapi/getSaldoTercDoctoLisa.p
Objetivo: retornar o saldo de terceiros de um docto da LISA.
Autor: Tadeu Silva
Data: 10/2024

*/
DEFINE INPUT  PARAMETER pNroDocto    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSeq         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER qtSaldo      AS DECIMAL     NO-UNDO.


DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cVlParam        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSeriePadrao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperPadrao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codEmitPadrao   AS INTEGER     NO-UNDO.




RUN esbo/boConsParam.p PERSIST SET hBoConsParam .

RUN getEmitPadrSaldoTercLisa      IN hBoConsParam(OUTPUT cVlParam) .
RUN getSeriePadrSaldoTercLisa     IN hBoConsParam(OUTPUT cSeriePadrao) .
RUN getNatOperPadrSaldoTercLisa   IN hBoConsParam(OUTPUT cNatOperPadrao) .

IF VALID-HANDLE(hBoConsParam) THEN
   DELETE PROCEDURE hBoConsParam.


ASSIGN codEmitPadrao = INT(cVlParam).



FIND saldo-terc NO-LOCK
    WHERE saldo-terc.cod-emitente   = codEmitPadrao
    AND   saldo-terc.nro-docto      = pNroDocto
    AND   saldo-terc.serie          = cSeriePadrao
    AND   saldo-terc.nat-operacao   = cNatOperPadrao
    AND   saldo-terc.it-codigo      = pItCodigo
    AND   saldo-terc.seq            = pSeq
    NO-ERROR.
IF AVAIL saldo-terc THEN DO:
   ASSIGN qtSaldo = saldo-terc.quantidade.

END.



