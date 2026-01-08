{esapi/ttRemessaTerc.i}
//NFs DE RETORNO planilha lisa
RUN esporadicos/getDadosPlanLisa.p(input-output TABLE ttRetorno).

DEFINE TEMP-TABLE ttDocto NO-UNDO
    FIELD nrDocto       AS  CHAR
    FIELD logLancado    AS LOGICAL
    INDEX pri IS PRIMARY nrDocto.
FOR EACH ttRetorno.
    FIND LAST docum-est
        WHERE docum-est.serie = '2'
        AND   int(docum-est.nro-docto) = int(ttRetorno.nrNota)
        AND docum-est.cod-estabel = '505'
        AND docum-est.cod-emitente = 38284
        AND docum-est.ce-atual
        NO-ERROR.
    FIND ttDocto
           WHERE ttDocto.nrDocto = ttRetorno.nrNota NO-ERROR.
    IF NOT AVAIL ttDocto THEN DO:
          CREATE ttDocto.
          ASSIGN ttdocto.nrdocto = ttRetorno.nrNota .
     END.
     ASSIGN ttdocto.logLancado = AVAIL docum-est.
END.
OUTPUT TO c:\temp\nfs_erp.txt.
FOR EACH ttDocto:
    EXPORT DELIMITER "|" ttDocto.nrDocto ttDocto.logLancado.
END.
