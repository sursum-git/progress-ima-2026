/*
programa:esapi/getSaldoTercDocto.p
Objetivo: retornar o saldo de terceiros de um docto.
Autor: Tadeu Silva
Data: 03/2024

*/
DEFINE INPUT  PARAMETER pCodEmitente AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNroDocto    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNatOperacao AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSeq         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER qtSaldo      AS DECIMAL     NO-UNDO.

FIND saldo-terc NO-LOCK
    WHERE saldo-terc.cod-emitente   = pCodEmitente
    AND   saldo-terc.nro-docto      = pNroDocto
    AND   saldo-terc.serie          = pSerie
    AND   saldo-terc.nat-operacao   = pNatOperacao
    AND   saldo-terc.it-codigo      = pItCodigo
    AND   saldo-terc.seq            = pSeq
    NO-ERROR.
IF AVAIL saldo-terc THEN DO:
   ASSIGN qtSaldo = saldo-terc.quantidade.

END.



