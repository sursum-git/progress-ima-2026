OUTPUT TO c:\temp\bkp-fat-dp22223.txt.
FOR EACH medtst.fat-duplic
    WHERE medtst.fat-duplic.dt-emissao >= 11.03.2017
    AND medtst.fat-duplic.dt-emissao   <= 11.06.2017
    AND  medtst.fat-duplic.nr-fatura <> '0088276'
    AND  medtst.fat-duplic.nr-fatura <> '0088277':
    //EXPORT medtst.fat-duplic.
    //DELETE medtst.fat-duplic.
    CREATE ems2med.fat-duplic.
    BUFFER-COPY medtst.fat-duplic TO ems2med.fat-duplic.

END.
