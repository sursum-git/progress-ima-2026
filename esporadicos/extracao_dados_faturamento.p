OUTPUT TO c:\temp\itens_vendidos.txt.
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel = '5'
    AND nota-fiscal.dt-cancel = ?
    AND nota-fiscal.dt-emis-nota >= 01.01.2017.
    FIND FIRST natur-oper OF nota-fiscal
        WHERE natur-oper.emite-duplic = YES
        NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.
    FIND emitente OF nota-fiscal NO-LOCK NO-ERROR.
    FIND gr-cli OF emitente NO-LOCK NO-ERROR.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND ITEM OF it-nota-fisc NO-LOCK NO-ERROR.
        FIND fam-comerc OF ITEM NO-LOCK NO-ERROR.
        FIND familia OF ITEM NO-LOCK NO-ERROR.
        EXPORT DELIMITER "|" 
        nota-fiscal.nr-nota-fis
        nota-fiscal.serie
        nota-fiscal.cod-estabel
        nota-fiscal.dt-emis-nota
        year(nota-fiscal.dt-emis-nota)
        MONTH(nota-fiscal.dt-emis-nota)
        DAY(nota-fiscal.dt-emis-nota)
        nota-fiscal.cod-emitente
        nota-fiscal.nome-abrev
        emitente.nome-emit
        emitente.estado
        emitente.cidade
        emitente.linha-produt
        emitente.atividade
        emitente.categoria
        gr-cli.descr 
        ITEM.it-codigo
        ITEM.desc-item
        ITEM.un
        fam-comerc.descr
        familia.descr
        it-nota-fisc.qt-faturada[1]
        it-nota-fisc.vl-preori
        it-nota-fisc.vl-preuni
        it-nota-fisc.vl-tot-item .




    END.

END.
OUTPUT CLOSE.
