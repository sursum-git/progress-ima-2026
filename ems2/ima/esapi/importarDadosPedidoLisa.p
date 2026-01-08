/********************************************************************************************
Programa    : esapi/importarDadosRetornoLisa
Autor       : Tadeu Silva Parreiras
Objetivo    : A partir das tabelas temporarias passadas por parametro
              incluir os dados do pedido LISA, assim como do seus itens e etq por itens
Data        : 01/2024 
Modificacoes:
*********************************************************************************************/
{lisa/extrairTtJsonPrePedido.i}

DEFINE TEMP-TABLE ttPedItemAux     LIKE ttPedItemFat.
DEFINE TEMP-TABLE ttPedItemEtqAux  LIKE ttPedItemEtq.

DEFINE INPUT PARAMETER  TABLE FOR ttPedido .
DEFINE INPUT PARAMETER  TABLE FOR ttPedItem .
DEFINE INPUT PARAMETER  TABLE FOR ttPedItemFat .
DEFINE INPUT PARAMETER  TABLE FOR ttPedItemEtq .


//bos para inclus∆o dos dados do pedido, item do pedido e etq do item da LISA
DEFINE VARIABLE hBoLisa10 AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBolisa11 AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoLisa12 AS HANDLE      NO-UNDO.


DEFINE VARIABLE idPedLisa     LIKE pedidos_lisa.pedido_lisa_id           NO-UNDO.
DEFINE VARIABLE idPedItemLisa LIKE itens_pedido_lisa.ITEM_pedido_lisa_id NO-UNDO.

{esp/util.i}
{esp/exportarTabelacsv3.i ttPedItemEtq " " " " "  "ttPedItemEtq_importarDAdosPedidoLisa" }

RUN esbo/boLisa10.p PERSIST SET hBoLisa10.
RUN iniciar IN hBoLisa10.

RUN esbo/boLisa11.p PERSIST SET hBoLisa11.
RUN iniciar IN hBoLisa11.

RUN esbo/boLisa12.p PERSIST SET hBoLisa12.
RUN iniciar IN hBoLisa12.

RUN inclusaoExtracaoTTJson IN hBoLisa10(INPUT TABLE ttPedido).
RUN incluir IN hBoLisa10.
RUN getIDRegCorrente IN hBoLisa10(OUTPUT idPedLisa).

RUN limparDados.

FOR EACH ttPedItemFat:
    EMPTY TEMP-TABLE ttPedItemAux.
    CREATE ttPedItemAux.
    BUFFER-COPY ttPedItemFat TO ttPedItemAux.
    RUN setIdPai IN hBoLisa11(idPedLisa).
    RUN inclusaoExtracaoTTJson IN hBoLisa11(INPUT TABLE ttPedItemAux).
    RUN incluir IN hBoLisa11.
    RUN getIDRegCorrente IN hBoLisa11(OUTPUT idPedItemLisa).
    FOR EACH ttPedItemEtq
        WHERE ttPedItemEtq.itCodigo     = ttPedItemFat.itCodigo
        AND   ttPedItemEtq.codRefer     = ttPedItemFat.codRefer
        USE-INDEX ind-item-ref.
        EMPTY TEMP-TABLE ttPedItemEtqAux.
        CREATE ttPedItemEtqAux.
        BUFFER-COPY ttPedItemEtq TO ttPedItemEtqAux.
        RUN setIdPai IN hBoLisa12(idPedItemLisa).
        RUN inclusaoExtracaoTTJson IN hBoLisa12(INPUT TABLE ttPedItemEtqAux).    
        RUN incluir IN hBoLisa12.
    END.
END.

RUN finalizar IN hBoLisa10.
RUN finalizar IN hBoLisa11.
RUN finalizar IN hBoLisa12.


PROCEDURE limparDados:

    FOR EACH ttPedITem
        WHERE ttPedItem.itCodigo = ''.
        DELETE ttPedItem.
    END.

    FOR EACH ttPedITemFat
        WHERE ttPedItemFat.itCodigo = ''.
        DELETE ttPedItemFat.
    END.



    FOR EACH ttPedITemEtq
        WHERE ttPedItemEtq.itCodigo = ''.
        DELETE ttPedItemEtq.
    END.

END PROCEDURE.







