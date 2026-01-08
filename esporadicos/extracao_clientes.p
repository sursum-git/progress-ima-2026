DEFINE VARIABLE lAchou AS LOGICAL     NO-UNDO.

OUTPUT TO c:\temp\emitente.txt.
FOR EACH emitente NO-LOCK:
    ASSIGN laChou = NO.
    FOR FIRST nota-fiscal OF emitente,
        EACH natur-oper OF nota-fiscal
        WHERE natur-oper.tp-rec-desp = 1 
        AND natur-oper.tipo-compra <> 3 .
        ASSIGN lAchou = YES.
    END.                    
    FIND gr-cli OF emitente NO-LOCK NO-ERROR .
    FIND FIRST ext-emitente
        OF emitente NO-LOCK NO-ERROR.
    FIND ramo-ativ OF ext-emitente NO-LOCK NO-ERROR.

    EXPORT DELIMITER "|" 
        emitente.cod-emitente 
        emitente.nome-emit
        emitente.cidade 
        emitente.estado 
        emitente.atividade 
        emitente.data-implant
        emitente.cod-gr-cli
        IF AVAIL gr-cli THEN gr-cli.descr  ELSE ''
        {adinc/i10ad098.i 4 emitente.ind-cre-cli}
        emitente.categoria  
        IF AVAIL ramo-ativ THEN  ramo-ativ.descricao ELSE 'NAO INFORMADO'
        IF AVAIL ext-emitente  AND ext-emitente.situacao <> 0 THEN  entry(ext-emitente.situacao,'Ativo,Inativo,Novo,Resgatado,Frequente',",") ELSE 'NAO INFORMADA'
        IF AVAIL ext-emitente THEN ext-emitente.cred-so-cartao ELSE NO
        IF AVAIL ext-emitente THEN ext-emitente.dt-ult-fat-med ELSE ?
        IF AVAIL ext-emitente THEN ext-emitente.LOG_varejo ELSE NO
        IF AVAIL ext-emitente THEN ext-emitente.log_atacado ELSE NO
        IF AVAIL ext-emitente THEN ext-emitente.LOG_industria ELSE NO
        IF AVAIL ext-emitente THEN ext-emitente.LOG_servico  ELSE NO
        emitente.cod-rep.

END.




OUTPUT CLOSE.
