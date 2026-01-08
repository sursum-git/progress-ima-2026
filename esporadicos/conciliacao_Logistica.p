DEFINE VARIABLE cItens AS CHARACTER   NO-UNDO FORMAT 'x(80)'.
DEFINE TEMP-TABLE tt
    FIELD data      AS DATE
    FIELD documento AS CHAR FORMAT 'x(20)'
    FIELD serie     AS CHAR
    FIELD emitente  AS INT
    FIELD nome_emit AS CHAR FORMAT 'x(12)'
    FIELD tipoTrans AS CHAR
    FIELD especie   AS CHAR
    FIELD quant     AS DECIMAL
    FIELD valor     AS DECIMAL
    FIELD itCodigo  AS CHAR FORMAT 'x(20)'
    FIELD codRefer  AS CHAR
    FIELD deposito  AS CHAR
    FIELD vlMedio   AS DECIMAL
    FIELD natureza  AS CHAR .

UPDATE cITens WITH WIDTH 550.

FOR EACH movto-estoq
    WHERE   LOOKUP(movto-estoq.it-codigo,cItens) > 0
    AND  cod-estabel = '5' :
    /*FIND FIRST docum-est OF movto-estoq NO-LOCK NO-ERROR.
    FIND FIRST item-doc-est OF docum-est
         WHERE item-doc-est.sequencia = movto-estoq.num-sequen
    NO-LOCK NO-ERROR.*/
    FIND FIRST emitente OF movto-estoq  NO-LOCK NO-ERROR.

    CREATE tt.
    ASSIGN  tt.data       = movto-estoq.dt-trans 
            tt.documento  = movto-estoq.nro-docto
            tt.serie      = movto-estoq.serie-docto
            tt.emitente   = movto-estoq.cod-emitente
            tt.nome_emit  = IF AVAIL emitente THEN emitente.nome-emit ELSE ''
            tt.tipoTrans  = IF movto-estoq.tipo-trans = 1 THEN 'entrada' ELSE 'saida'
            tt.especie    = {ininc/i03in218.i 4 movto-estoq.esp-docto}
            tt.quant      = IF movto-estoq.tipo-trans = 1 THEN movto-estoq.quantidade ELSE movto-estoq.quantidade * -1
            tt.valor      = IF movto-estoq.tipo-trans = 1 THEN movto-estoq.valor-mat-m[1] ELSE movto-estoq.valor-mat-m[1]  * -1
            tt.itCodigo   = movto-estoq.it-codigo
            tt.codRefer   = movto-estoq.cod-refer
            tt.deposito   = movto-estoq.cod-depos
            tt.natureza   = movto-estoq.nat-operacao.
    IF movto-estoq.esp-docto = 22 AND tt.quant = 0 THEN DO:
      FIND FIRST nota-fiscal 
            WHERE  nota-fiscal.cod-estabel = movto-estoq.cod-estabel
            AND    nota-fiscal.nr-nota-fis = movto-estoq.nro-docto
            AND    nota-fiscal.serie       = movto-estoq.serie-docto
            NO-LOCK NO-ERROR.
      IF AVAIL nota-fiscal THEN DO:
         FIND FIRST it-nota-fisc OF nota-fiscal
             WHERE it-nota-fisc.it-codigo  = movto-estoq.it-codigo
             AND   it-nota-fisc.cod-refer  = movto-estoq.cod-refer
             AND   it-nota-fisc.nr-seq-fat = movto-estoq.num-sequen
             NO-LOCK  NO-ERROR.
         IF AVAIL it-nota-fisc  THEN
            ASSIGN tt.quant = IF movto-estoq.tipo-trans = 1 THEN it-nota-fisc.qt-faturada[1] ELSE it-nota-fisc.qt-faturada[1]  * -1 .
      END.
    END.
    ASSIGN tt.vlMedio = tt.valor / tt.quant .
END.
OUTPUT TO c:\temp\conciliacao.txt.
FOR EACH tt:
    EXPORT DELIMITER "|" tt.
END.
OUTPUT CLOSE.
