/* +++ definicao da biblioteca de funcoes +++ */

{sop\solib001.i}

PROCEDURE retornarTiposAtiv:
DEFINE OUTPUT PARAMETER cLista AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cValorFixo AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
FOR EACH tipos_atividade NO-LOCK:
    ASSIGN  cValorFixo = tipos_atividade.desc_tipo_atividade + "," + string(tipos_atividade.cod_tipo_atividade)
            cLista = IF cLista = '' THEN cValorFixo
                     ELSE cLista + "," + cValorFixo.
END.


END PROCEDURE.
