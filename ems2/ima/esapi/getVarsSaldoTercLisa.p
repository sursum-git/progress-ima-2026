/*

programa: esapi/getVarsSaldoTercLisa.p
objetivo: retornar as variaveis que fazem parte da busca de terceiros da LISA
data:10/2024
autor: tadeu silva
*/

DEFINE OUTPUT PARAMETER cSeriePadrao    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cNatOperPadrao  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER codEmitPadrao   AS INTEGER     NO-UNDO.

DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cVlParam        AS CHARACTER   NO-UNDO.

RUN esbo/boConsParam.p PERSIST SET hBoConsParam .
RUN getEmitPadrSaldoTercLisa      IN hBoConsParam(OUTPUT cVlParam) .
RUN getSeriePadrSaldoTercLisa     IN hBoConsParam(OUTPUT cSeriePadrao) .
RUN getNatOperPadrSaldoTercLisa   IN hBoConsParam(OUTPUT cNatOperPadrao) .
ASSIGN codEmitPadrao = INT(cVlParam).
