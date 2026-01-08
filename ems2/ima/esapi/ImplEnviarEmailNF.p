FIND nota-fiscal NO-LOCK
    WHERE nota-fiscal.cod-estabel   = '505'
    AND   nota-fiscal.serie         = '2'
    AND   nota-fiscal.nr-nota-fis   = '0013255'
    NO-ERROR
    .
RUN esapi/enviarEmailNF.p(ROWID(nota-fiscal)).
