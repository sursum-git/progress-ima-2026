DEFINE VARIABLE lcXml AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lOk AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE ttReg NO-UNDO LIKE retornos_lisa.
FOR EACH retornos_lisa WHERE dt_nf = ? :
    DISP retornos_lisa.nr_pedido.
    COPY-LOB retornos_lisa.conteudo_xml TO lcXml.
    RUN esapi/extrairDadosXmlNfRetorno.p(INPUT-OUTPUT TABLE ttReg,
                                         lcXml,
                                         OUTPUT lOk,
                                         OUTPUT cErro).
    IF lOk THEN DO:
       FOR EACH ttReg.
           ASSIGN retornos_lisa.dt_nf = ttReg.dt_nf.
       END.
    END.
END.
