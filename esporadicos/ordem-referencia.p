DEFINE VARIABLE iOrdem AS INTEGER     NO-UNDO.
FOR EACH referencia.
        RUN esapi/getOrdemRef.p(referencia.cod-refer,OUTPUT iOrdem).
        ASSIGN referencia.int-2 = iOrdem.
        DISP referencia.cod-refer referencia.int-2 .
        PAUSE 0. 

END.
