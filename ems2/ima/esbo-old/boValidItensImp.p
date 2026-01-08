/**************************************************************************          
programa :esbo/boValidItensImp
Objetivo: Fazer todas valida‡äes necess rias aos itens importados comprados
at‚ o processo de disponibiliza‡Æo para venda dos mesmos.
**************************************************************************/
DEFINE VARIABLE itCodigo            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE itemProcCompraId    AS INTEGER     NO-UNDO.
DEFINE VARIABLE itemContainerId     AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBoMsg              AS HANDLE      NO-UNDO.
{esbo/boMsg.i}
PROCEDURE iniciarBos:

    RUN esbo/boMsg.p PERSIST SET hBoMsg .

END PROCEDURE.

PROCEDURE finalizarBos:

    IF VALID-HANDLE(hBoMsg) THEN
       DELETE PROCEDURE hBoMsg.


END PROCEDURE.


PROCEDURE validarItemProcCompra:

    DEFINE INPUT  PARAMETER pID AS INTEGER     NO-UNDO.
    FIND itens_proc_compra 
        WHERE itens_proc_compra.ITEM_proc_compra_id = pId
        NO-LOCK NO-ERROR.
    IF NOT AVAIL itens_proc_compra THEN DO:
       RUN setMsg IN hBoMsg(1,'NÆo foi encontrado item do pedido de importa‡Æo com o ID' + STRING(pId),'erro').
    END.
    ELSE DO:
        IF itens_proc_compra.codigo_erp = '' THEN DO:
           RUN setMsg IN hBoMsg(2,'O C¢digo do item ERP est  em branco para  o item do pedido de importa‡Æo com o ID' + STRING(pId),'erro').
        END.
    END.    
END PROCEDURE.

PROCEDURE validarItemContainerId:

    DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNrContainer    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lAchou                  AS LOGICAL     NO-UNDO.

    FIND  pp-container
        WHERE pp-container.nr-container = pNrContainer
        NO-LOCK NO-ERROR.
    IF NOT AVAIL pp-container THEN DO:
       RUN setMsg IN hBoMsg(3,'O container:' + STRING(pNrContainer)  + ' NÆo foi encontrado ','erro').
    END.
    ELSE DO:
        FOR FIRST itens_container
            WHERE  itens_container.container_id = pNrContainer
            NO-LOCK,
            EACH itens_proc_compra OF ITEns_container 
            WHERE itens_proc_compra.codigo_erp = pItCodigo NO-LOCK.
            ASSIGN lAchou = YES.
        END.                    
    END.
    
    IF lAchou = NO THEN
       RUN setMsg IN  hBoMsg(3,'Aloca‡Æo item(' + pItCodigo + ') X container(' + string(pNrContainer) + ') nÆo encontrada','erro').
        


END PROCEDURE.

PROCEDURE getTTMsg:

    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttMsg.

    RUN getTTMsg IN hBoMsg(OUTPUT TABLE ttMsg).


END PROCEDURE.














