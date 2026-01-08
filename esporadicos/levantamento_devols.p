DEFINE TEMP-TABLE tt
        FIELD codEmitente AS INT
        FIELD serieDocto  AS CHAR
        FIELD nrDocto     AS CHAR FORMAT 'x(10)'
        FIELD dtEmissao   AS DATE
        FIELD natOperacao AS CHAR FORMAT 'x(12)'
        FIELD nrNotaDevol AS CHAR FORMAT 'x(12)'
        FIELD nrSeqCompra AS INT
        FIELD nrSeqVenda  AS INT
        FIELD dtDevol     AS DATE
        FIELD itCodigo    AS CHAR FORMAT 'x(20)'
        FIELD codRefer    AS CHAR 
        FIELD quantidade  AS DECIMAL
        FIELD valor       AS DECIMAL
        FIELD tipo        AS CHAR.

DEFINE TEMP-TABLE ttDevol
    FIELD   nrDocto AS CHAR FORMAT 'x(10)'
    FIELD   vlDevol AS DECIMAL
    FIELD   vlNC    AS DECIMAL
    FIELD   vlAF    AS DECIMAL.


FOR EACH docum-est NO-LOCK
    WHERE docum-est.cod-estabel = '1'
    AND   docum-est.serie-docto = '3'
    AND   docum-est.cod-emitente = 10535:
    FIND FIRST nota-fiscal 
        WHERE nota-fiscal.cod-estabel =  docum-est.cod-estabel
        AND   nota-fiscal.serie       =  docum-est.serie-docto
        AND   nota-fiscal.nr-nota-fis =  docum-est.nro-docto 
        AND   nota-fiscal.cod-emitente = docum-est.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       IF nota-fiscal.dt-cancel <> ? THEN NEXT.

    FOR EACH item-doc-est OF docum-est NO-LOCK:
        CREATE tt.
        ASSIGN tt.tipo = 'compra'
               tt.codEmitente  = docum-est.cod-emitente
               tt.serieDocto   = docum-est.serie-docto
               tt.nrDocto      = docum-est.nro-docto
               tt.dtEmissao    = docum-est.dt-trans
               tt.natOperacao  = docum-est.nat-operacao
               tt.itCodigo     = item-doc-est.it-codigo
               tt.codRefer     = item-doc-est.cod-refer
               tt.quantidade   = item-doc-est.quantidade
               tt.valor        = item-doc-est.preco-total[1]
               tt.nrSeqCompra  = item-doc-est.sequencia.
        FOR EACH devol-forn OF item-doc-est NO-LOCK:
            CREATE tt.
            ASSIGN tt.tipo = 'devolucao'
               tt.codEmitente  = devol-forn.cod-emitente
               tt.serieDocto   = devol-forn.serie-docto
               tt.nrDocto      = devol-forn.nro-docto
               tt.nrNotaDevol  = devol-forn.nr-nota-fis
               tt.dtDevol      = devol-forn.dt-devol
               tt.natOperacao  = devol-forn.nat-operacao
               tt.itCodigo     = devol-forn.it-codigo
               tt.codRefer     = ''
               tt.quantidade   = devol-forn.qt-devol
               tt.valor        = devol-forn.vl-devol
               tt.nrSeqCompra  = item-doc-est.sequencia
               tt.nrSeqVenda   = devol-forn.nr-seq-fat .

        END.
    END.
END.
OUTPUT TO c:\temp\tt.txt.

PUT "EMITENTE|SERIE|DOCUMENTO|DT.EMISSAO|NAT.OPERACAO|NOTA DEVOLUCAO|SEQ.COMPRA|SEQ.VENDA|DT.DEVOLUCAO|ITEM|REFERENCIA|QUANTIDADE|VALOR|TIPO" SKIP.       

FOR EACH tt
    WHERE tt.tipo = 'devolucao'
    AND   tt.dtDevol >= 10.06.2017:
    EXPORT DELIMITER "|" tt.
    FIND FIRST ttDevol
        WHERE ttDevol.nrDocto = tt.nrDocto NO-ERROR.
    IF NOT AVAIL ttDevol THEN DO:
       CREATE ttDevol.
       ASSIGN ttDevol.nrDocto = tt.nrDocto.
    END.
    ASSIGN ttDevol.vlDevol = ttDevol.vlDevol +  tt.valor.
END.
OUTPUT CLOSE.


FOR EACH ttDevol:
    FOR EACH tit_ap NO-LOCK
        WHERE tit_ap.cod_tit_ap = ttDevol.nrDocto
        AND   tit_ap.cod_espec_docto = 'NC'.
       ASSIGN ttDevol.vlNC = ttDevol.vlNC + tit_ap.val_origin_tit_ap.
    END.
    
    FOR EACH tit_ap NO-LOCK
        WHERE tit_ap.cod_tit_ap = ttDevol.nrDocto
        AND   tit_ap.cod_espec_docto = 'AF'.
       ASSIGN ttDevol.vlAF = ttDevol.vlAF + tit_ap.val_origin_tit_ap.
    END.
END.


OUTPUT TO c:\temp\devolNF.txt.
PUT "DOCUMENTO|VALOR DEVOLVIDO|VALOR NC|VALOR AF" SKIP.
FOR EACH ttDevol :
    EXPORT DELIMITER "|" ttDevol.
END.

OUTPUT CLOSE.
/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 serie-docto                      char        im
   20 nro-docto                        char        im
   30 cod-emitente                     inte        im
   40 nat-operacao                     char        im
   50 sequencia                        inte        im
   60 cod-estabel                      char        im
   70 serie                            char        im
   80 nr-nota-fis                      char        im
   90 nr-seq-fat                       inte        im
  100 codigo-rejei                     inte
  110 dt-devol                         date
  120 qt-devol                         deci-4
  130 vl-devol                         deci-2
  140 char-1                           char
  150 char-2                           char
  160 dec-1                            deci-8
  170 dec-2                            deci-8
  180 int-1                            inte
  190 int-2                            inte
  200 log-1                            logi
  210 log-2                            logi
  220 data-1                           date
  230 data-2                           date
  240 it-codigo                        char        im
  250 vl-devol-me                      deci-2
*/
