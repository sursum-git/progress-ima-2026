DEFINE BUFFER bfParam FOR PARAM_nat_operacao.
FOR EACH PARAM_nat_operacao :
    FIND FIRST bfParam
        WHERE bfParam.cod_param_nat_operacao_pai = PARAM_nat_operacao.cod_param_nat_operacao
        NO-LOCK NO-ERROR.
    IF AVAIL bfParam THEN
       ASSIGN PARAM_nat_operacao.cod_nat_operacao = ''.
END.
