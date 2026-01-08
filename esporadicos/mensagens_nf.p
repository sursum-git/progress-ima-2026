DEFINE VARIABLE cMsg LIKE mensagem.texto-mens NO-UNDO.
OUTPUT TO c:\temp\nf_msg.txt.
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= 01.01.2017 
    AND   nota-fiscal.dt-cancel <> ?.
    FIND natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
    FIND mensagem OF natur-oper NO-LOCK NO-ERROR.
    ASSIGN cMsg = REPLACE(mensagem.texto-mens,CHR(10),'<br>').
    IF AVAIL mensagem THEN
      EXPORT DELIMITER "|" natur-oper.nat-operacao 
        natur-oper.denominacao
        natur-oper.cod-cfop  cMsg.


END.
OUTPUT CLOSE.
