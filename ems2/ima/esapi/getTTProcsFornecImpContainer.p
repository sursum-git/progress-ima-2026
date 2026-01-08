/******************************************************************************
**  Programa: esapi/getTTProcsFornecImpContainer.p
**  Objetivo: Retornar uma TT com os codigos de processo de fornecedor
    referentes a determinado container, separados por virgula.
**  Data: 08/2025
**  Autor:Tadeu silva
******************************************************************************/
{esapi/getTTProcsFornecImpContainer.i}
DEFINE INPUT  PARAMETER pNrContainer AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttProc.

FOR EACH itens_container FIELDS(container_id item_proc_compra_id) NO-LOCK,
    EACH itens_proc_compra OF itens_container NO-LOCK,
    EACH procs_compra  NO-LOCK OF itens_proc_compra
    WHERE itens_container.container_id = pNrContainer :
    FIND FIRST ttProc 
        WHERE ttProc.proc_compra_id = procs_compra.proc_compra_id NO-ERROR.
    IF NOT AVAIL ttProc THEN DO:
       CREATE ttProc.
       ASSIGN ttProc.proc_compra_id = procs_compra.proc_compra_id
              ttProc.procFornec     = procs_compra.cod_proc_fornec
             .        
    END.  
    
END.




