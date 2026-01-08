/**************************************************************************
Programa: esapi/verifExistEtqContainerNoEstoque.p 
Autor: Tadeu Silva Parreiras
Objetivo: verificar se para o container informado por parametro existe ainda
alguma etiqueta no estoque.
Data: 05/2024
Modificacoes:
*****************************************************************************/


DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER lTemEtqEstoq AS LOGICAL     NO-UNDO.

IF CAN-FIND(FIRST ob-etiqueta
    WHERE ob-etiqueta.nr-container = pNrContainer
    AND   ob-etiqueta.situacao = 3 //em estoque  
    ) THEN DO:
    ASSIGN  lTemEtqEstoq = YES.
END.
ELSE 
    ASSIGN  lTemEtqEstoq = NO.

