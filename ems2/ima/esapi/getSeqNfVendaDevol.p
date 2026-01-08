/***********************************************************
programa: esapi/getSeqNfVendaDevol.p
objetivo: buscar o rowid da sequencia da nf venda origem da devolu‡Æo
data: 02/2025
**********************************************************/
DEFINE INPUT  PARAMETER pCodEstabel  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNf          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEmitente AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNatOperacao AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSequencia   AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER pRowid       AS ROWID       NO-UNDO.

/*MESSAGE 'devolucao - busca venda'
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

FOR FIRST devol-cli FIELDS(cod-estabel serie nr-nota-fis nr-sequencia) NO-LOCK
    WHERE devol-cli.serie-docto     = pSerie
    AND   devol-cli.nro-docto       = pNf
    AND   devol-cli.cod-emitente    = pCodEmitente
    AND   devol-cli.nat-operacao    = pNatOperacao   
    AND   devol-cli.sequencia       = pSequencia
    USE-INDEX ch-nfe .
END.

IF AVAIL devol-cli THEN DO:
   /*MESSAGE  'nota de venda:'  devol-cli.nr-nota-fis
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   FOR FIRST nota-fiscal FIELDS(cod-estabel serie nr-nota-fis ) NO-LOCK
        WHERE nota-fiscal.cod-estabel =  devol-cli.cod-estabel
        AND   nota-fiscal.serie       =  devol-cli.serie
        AND   nota-fiscal.nr-nota-fis =  devol-cli.nr-nota-fis,
        EACH it-nota-fisc OF nota-fiscal NO-LOCK
        WHERE it-nota-fisc.nr-seq-fat = devol-cli.nr-sequencia.         
    END.
    IF AVAIL nota-fiscal THEN DO:
       ASSIGN pRowid = ROWID(it-nota-fisc).         
    END.
END.
