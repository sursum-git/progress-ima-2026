DEFINE BUFFER bf  FOR ems5.representante.
DEFINE TEMP-TABLE tt 
    FIELD codigo AS INT
    FIELD empresa AS CHAR FORMAT 'x(50)'
    FIELD situacao AS CHAR FORMAT 'x(50)'.

/*DEFINE TEMP-TABLE tt LIKE ems5.representante.  */
DEFINE VARIABLE cCaminho AS CHARACTER   NO-UNDO INIT 'c:\temp\representantes.txt'.
FOR EACH ems5ima.representante:
    CREATE tt.
    ASSIGN tt.codigo  = ems5ima.representante.cdn_repres
           tt.empresa = ems5ima.representante.cod_empresa.
    FIND FIRST ems5.representante
        WHERE ems5.representante.cdn_repres = ems5ima.representante.cdn_repres
        AND   ems5.representante.cod_empresa = ems5ima.representante.cod_empresa NO-ERROR.
    IF NOT AVAIL ems5.representante THEN DO:
       ASSIGN tt.situacao = "criado".
       CREATE ems5.representante.
       BUFFER-COPY ems5ima.representante TO ems5.representante.
       FOR EACH ems5ima.repres_financ OF ems5ima.representante NO-LOCK:
           CREATE ems5.repres_financ.
           BUFFER-COPY ems5ima.repres_financ TO ems5.repres_financ.
       END.
    END.
    ELSE DO:
        ASSIGN tt.situacao = "J  Cadastrado".
    END.
    PUT SKIP.
END.

OUTPUT TO VALUE(cCaminho) NO-CONVERT .
FOR EACH tt:
    DISP tt WITH WIDTH 550.
END.
OUTPUT CLOSE.

OS-COMMAND SILENT value("start notepad " + cCaminho)   .
