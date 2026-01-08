/***************************************************************************************
programa: esbo/boAltPedVenda.p
objetivo: Controlar s altera‡äes do pedido de venda, possibilitando
o controle de cancelamentos por diminui‡Æo, assim como as metricas
necessarias para avalia‡Æo da necessidade de aprova‡Æo do pedido
ap¢s algumas altera‡äes.
****************************************************************************************/

DEFINE BUFFER peds_web FOR tst.peds_web.
DEFINE BUFFER ped-venda-ext FOR tst.ped-venda-ext.

DEFINE TEMP-TABLE ttPedVendaAnt     LIKE ped-venda .
DEFINE TEMP-TABLE ttPedVendaAtu     LIKE ped-venda .   
DEFINE TEMP-TABLE ttPedVendaExtAnt  LIKE tst.ped-venda-ext .
DEFINE TEMP-TABLE ttPedVendaExtAtu  LIKE tst.ped-venda-ext .

DEFINE TEMP-TABLE ttPedWebAnt   LIKE tst.peds_web .
DEFINE TEMP-TABLE ttPedWebAtu   LIKE tst.peds_web .
//DEFINE VARIABLE  cTipoRegCorrente AS CHARACTER   NO-UNDO.

DEFINE VARIABLE idTransacao AS INT64 NO-UNDO.


PROCEDURE setIDTransacao:

    DEFINE INPUT  PARAMETER pTransacao AS INT64  NO-UNDO.
    ASSIGN idTransacao = pTransacao.

END PROCEDURE.

/************************************************************************
verificar a necessidade
PROCEDURE setTipoRegCorrente:
    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.
    ASSIGN cTipoRegCorrente = pTipo. //anterior ou atual 


END PROCEDURE.

PROCEDURE setVlCpPedVenda:
    DEFINE INPUT  PARAMETER cCampo  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER vlCampo AS CHARACTER   NO-UNDO.
    IF cTipoRegCorrente = 'anterior' THEN DO:
        FIND FIRST ttPedVendaAnt NO-ERROR.
        IF NOT AVAIL ttPedVendaAnt THEN
           CREATE ttPedVendaAnt.
    END.
    ELSE DO:
        FIND FIRST ttPedVendaAtu NO-ERROR.
        IF NOT AVAIL ttPedVendaAtu THEN
           CREATE ttPedVendaAtu.
    END.

    CASE cCampo:

    WHEN '' THEN

        
    END CASE.

END PROCEDURE.
***********************************************************************************/

PROCEDURE setTTPedVendaAnt:

DEFINE INPUT PARAMETER TABLE FOR ttPedVendaAnt.

END PROCEDURE.

PROCEDURE setTTPedWebAnt:


END PROCEDURE.


PROCEDURE setTTPedVendaExtAnt:
    DEFINE INPUT PARAMETER TABLE FOR ttPedVendaExtAnt.




END PROCEDURE.

PROCEDURE setTTPedVendaExtAtu:
    DEFINE INPUT PARAMETER TABLE FOR ttPedVendaAtu.



END PROCEDURE.

PROCEDURE compVlTotPed:


END PROCEDURE.



PROCEDURE setTTPedWebAtual:



END PROCEDURE.


PROCEDURE setTTPedVendaAtual:

DEFINE INPUT PARAMETER TABLE FOR ttPedVendaAtu.

END PROCEDURE.

PROCEDURE verificarDifs:



END PROCEDURE.


