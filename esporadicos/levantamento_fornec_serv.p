/*MESSAGE {ininc/i03in090.i 03}
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
DEFINE TEMP-TABLE tt
    FIELD codEmitente AS  INT
    FIELD nomeAbrev   LIKE emitente.nome-abrev
    FIELD nomeEmit    LIKE emitente.nome-emit 
    FIELD cnpj        AS CHAR FORMAT 'x(15)'
    FIELD cidade      AS CHAR FORMAT 'x(150)'
    FIELD estado      AS CHAR
    FIELD ano         AS INT
    FIELD mes         AS INT.
FOR EACH docum-est NO-LOCK
    WHERE docum-est.dt-trans >= 01.01.2017
    AND docum-est.cod-observa = 4.

    FIND FIRST emitente OF docum-est NO-LOCK NO-ERROR.

    FIND FIRST tt
        WHERE tt.codEmitente = docum-est.cod-emitente
        AND   tt.ano         = YEAR(docum-est.dt-trans)
        AND   tt.mes         = MONTH(docum-est.dt-trans)
        NO-LOCK NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
       ASSIGN tt.codEmitente = docum-est.cod-emitente       
              tt.ano         = YEAR(docum-est.dt-trans)
              tt.mes         = MONTH(docum-est.dt-trans)
              tt.nomeAbrev   = emitente.nome-abrev
              tt.nomeEmit    = emitente.nome-emit
              tt.cnpj        = emitente.cgc
              tt.cidade      = emitente.cidade
              tt.estado      = emitente.estado .
              

    END.
END.
OUTPUT TO c:\temp\fornec_Serv.csv.
PUT 'cod.emitente;nome abrev; nome emit.;cnpj;cidade;estado;ano;mes' SKIP.
FOR EACH tt.
    EXPORT DELIMITER ";" tt.


END.
OUTPUT CLOSE.
