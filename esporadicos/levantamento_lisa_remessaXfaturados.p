DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD itCodigo      AS CHAR FORMAT 'x(20)'
    FIELD codRefer      AS CHAR 
    FIELD qtEnviada     AS DECIMAL
    FIELD qtFaturada    AS DECIMAL
    FIELD qtSaldo       AS DECIMAL
    .


FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel   = '505'
    AND   nota-fiscal.dt-cancel     = ?
    AND   nota-fiscal.nat-operacao  = '59207i'
    AND   nota-fiscal.dt-emis-nota  >= 05.01.2023.
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        RUN sincrTTItem(it-nota-fisc.it-Codigo,it-nota-fisc.cod-refer,it-nota-fisc.qt-faturada[1],'enviada').
    END.        
END.

    
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel   = '505'
    AND   nota-fiscal.dt-cancel     = ?
    AND   nota-fiscal.nat-operacao  <> '59207i'
    AND   nota-fiscal.dt-emis-nota  >= 05.01.2023,
    EACH ped-venda NO-LOCK
    WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
    AND   ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
    .
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        RUN sincrTTItem(it-nota-fisc.it-Codigo,it-nota-fisc.cod-refer,it-nota-fisc.qt-faturada[1],'faturada').
    END.
END.

OUTPUT TO c:\temp\ttItem.csv.
FOR EACH ttITem:
    ASSIGN ttItem.qtSaldo = ttItem.qtEnviada - ttItem.qtFaturada.
    EXPORT DELIMITER ";" ttItem.
END.
OUTPUT CLOSE.

PROCEDURE sincrTTItem:

DEFINE INPUT  PARAMETER pItem  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pREf   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pQt    AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pCampo AS CHARACTER   NO-UNDO.

    FIND ttItem
        WHERE ttItem.itCodigo = pItem
        AND   ttItem.codRefer = pRef
        NO-ERROR.
    IF NOT AVAIL ttItem THEN DO:
       CREATE ttITem.
       ASSIGN ttITem.itCodigo  = it-nota-fisc.it-codigo
              ttItem.codRefer  = it-nota-fisc.cod-refer .
    END.
    CASE pCampo:
        WHEN 'enviada' THEN
            ASSIGN ttItem.qtEnviada  = ttItem.qtEnviada  + it-nota-fisc.qt-faturada[1].
        WHEN 'faturada' THEN
            ASSIGN ttItem.qtFaturada = ttItem.qtFaturada + it-nota-fisc.qt-faturada[1].


    END CASE.
    



END PROCEDURE.
