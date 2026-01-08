OUTPUT TO C:\TEMP\PEDNAT.TXT.
DEFINE VARIABLE cNat AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrPedido AS INTEGER     NO-UNDO.
UPDATE cnat nrpedido.
FOR EACH PED-VENDA
    WHERE ped-venda.nr-pedido = nrPedido
    AND PED-VENDA.COD-SIT-PED =1 
    //AND   NAT-OPERACAO = '51202M'
    .
    DISP DT-IMPLANT NR-PEDIDO.
    /*IF nr-pedido = 272511 OR nr-pedido = 272514 OR nr-pedido = 272674
        OR nr-pedido = 272915 
        OR nr-pedido =  272916
        THEN
        ASSIGN cNat = '51202I'.
    ELSE 
       ASSIGN cNat = '51202V'.
    */
    ASSIGN ped-venda.nat-operacao = cNat.
   
    FOR EACH ped-item OF ped-venda.
        ASSIGN ped-item.nat-operacao = cNat.
    END.
END.
