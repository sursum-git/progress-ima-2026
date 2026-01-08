/***************************************************
programa: esbo/boCalculos.p
objetivo: manter a tabela de calculos
****************************************************/
{esp/util.i}
DEFINE VARIABLE idCalculo AS INTEGER     NO-UNDO.
DEFINE BUFFER bfCalc FOR calculos .
PROCEDURE setCalculo:
    DEFINE INPUT  PARAMETER pCalculo AS INTEGER     NO-UNDO.
    ASSIGN idCalculo = pCalculo.
    FIND bfCalc
        WHERE bfCalc.calculo_id = idCalculo
        NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE getSufixo:
    DEFINE OUTPUT PARAMETER cSufixo AS CHARACTER   NO-UNDO.

    IF AVAIL bfCalc THEN
       ASSIGN cSufixo = bfCalc.sufixo.


END PROCEDURE.

PROCEDURE getDescricao:
    DEFINE OUTPUT PARAMETER cDescricao AS CHARACTER   NO-UNDO.
    IF AVAIL bfCalc THEN
       ASSIGN cDescricao = bfCalc.Descricao.

END PROCEDURE.

PROCEDURE getListaCb:
    DEFINE OUTPUT PARAMETER cLista AS CHARACTER   NO-UNDO.

    FOR EACH calculos NO-LOCK .

        RUN incrValor(INPUT-OUTPUT cLista, 
                      calculos.descricao + "," + STRING(calculos.calculo_id)     
                      , ",").
        

    END.

END PROCEDURE.



