DEFINE VARIABLE iLista AS INTEGER     NO-UNDO.
DEFINE VARIABLE cOpcao AS CHAR        NO-UNDO.
              
PROCEDURE setLista:
    DEFINE INPUT  PARAMETER pLista AS INTEGER     NO-UNDO.
    ASSIGN iLista = pLista.

END PROCEDURE.
              
              
PROCEDURE setOpcao:
    DEFINE INPUT  PARAMETER pOpcao AS CHAR     NO-UNDO.
    ASSIGN cOpcao = pOpcao.



END PROCEDURE.


PROCEDURE getDescrOpcao:
    DEFINE OUTPUT PARAMETER cDescricao AS CHARACTER   NO-UNDO.
    FIND FIRST opcoes_lista
        WHERE opcoes_lista.lista_id = iLista
        AND   opcoes_lista.cod_opcao = cOpcao
        NO-LOCK NO-ERROR.
    IF AVAIL opcoes_lista THEN
       ASSIGN cDescricao = opcoes_lista.desc_opcao .




END PROCEDURE.
