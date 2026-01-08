/*************************************************
programa: esapi/getDadosNfVendaDevol.p
objetivo: buscar dados da nota fiscal e da 
propor‡Æo da devolu‡Æo a partir dos dados
da nota fiscal de devolu‡Æo por item/referencia
data: 03/2024
**************************************************/
DEFINE INPUT  PARAMETER pCodEstabel  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNf          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEmitente AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNatOperacao AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSequencia   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pQtNf        AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER codEstabel   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER serie        AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER nrNotaFis    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER itCodigo     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER codRefer     AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER dPropDevol   AS DECIMAL     NO-UNDO.


FIND devol-cli NO-LOCK
    WHERE devol-cli.serie-docto     = pSerie
    AND   devol-cli.nro-docto       = pNf
    AND   devol-cli.cod-emitente    = pCodEmitente
    AND   devol-cli.nat-operacao    = pNatOperacao
    AND   devol-cli.sequencia       = pSequencia
    USE-INDEX ch-nfe NO-ERROR.

IF AVAIL devol-cli THEN DO:

    FIND it-nota-fisc 
         WHERE it-nota-fisc.cod-estabel =  devol-cli.cod-estabel
         AND   it-nota-fisc.serie       =  devol-cli.serie
         AND   it-nota-fisc.nr-nota-fis =  devol-cli.nr-nota-fis
         AND   it-nota-fisc.nr-seq-fat  =  devol-cli.nr-sequencia
        NO-LOCK NO-ERROR.
    IF AVAIL it-nota-fisc THEN DO:
       ASSIGN dPropDevol = pQtNf / it-nota-fisc.qt-faturada[1] .
       RUN setVals.
    END.                                         
END.
ELSE DO:
    FIND item-doc-est 
         WHERE item-doc-est.serie-docto     = pSerie          
         AND   item-doc-est.nro-docto       = pNf             
         AND   item-doc-est.cod-emitente    = pCodEmitente    
         AND   item-doc-est.nat-operacao    = pNatOperacao    
         AND   item-doc-est.sequencia       = pSequencia      
         NO-LOCK NO-ERROR.
    IF AVAIL item-doc-est THEN DO:
       FIND docum-est OF item-doc-est NO-LOCK NO-ERROR.
       FIND it-nota-fisc 
         WHERE it-nota-fisc.cod-estabel =  docum-est.cod-estabel
         AND   it-nota-fisc.serie       =  item-doc-est.serie-comp
         AND   it-nota-fisc.nr-nota-fis =  item-doc-est.nro-comp
         AND   it-nota-fisc.nr-seq-fat  =  item-doc-est.seq-comp
         AND   it-nota-fisc.it-codigo   =  item-doc-est.it-codigo
        NO-LOCK NO-ERROR. 
        IF AVAIL it-nota-fisc THEN DO:
           ASSIGN dPropDevol = pQtNf / it-nota-fisc.qt-faturada[1] .
           RUN setVals. 
        END.
    END.
    ELSE DO:
        RETURN 'nok'.
    END.
END.

PROCEDURE setVals:
 ASSIGN codEstabel  = it-nota-fisc.cod-estabel
         serie      = it-nota-fisc.serie
         nrNotaFis  = it-nota-fisc.nr-nota-fis
         itCodigo   = it-nota-fisc.it-codigo
         codRefer   = it-nota-fisc.cod-refer .
END PROCEDURE.
