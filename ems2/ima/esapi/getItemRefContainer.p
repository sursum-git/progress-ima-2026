/****************************************************************************
programa:esapi/getItemRefContainer.p
objetivo: Retornar os itens e referencias do container
para serem utilizados por outros programas.
autor:Tadeu Silva
data: 10/2025
*****************************************************************************/

{lisa/ttItemRef.i}

DEFINE INPUT  PARAMETER pContainer AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttItemRef.

FOR EACH pp-container NO-LOCK
    WHERE pp-container.nr-container = pContainer :
    FOR EACH pp-it-container OF pp-container NO-LOCK:
        CREATE ttItemRef.
        ASSIGN ttItemRef.itCodigo   = pp-it-container.it-codigo
               ttItemRef.codRefer   = pp-it-container.cod-refer
               .
    END.
END.
