OUTPUT TO c:\temp\lv01.csv.
FOR EACH ped-venda-ext FIELDS(cod_param_nat_operacao nr-pedido) NO-LOCK
WHERE lookup(string(ped-venda-ext.cod_param_nat_operacao),'189,485,207,342,394,470,484') > 0,
EACH ped-venda FIELDS(dt-implant nome-abrev nr-pedcli nr-pedido) NO-LOCK
    WHERE ped-venda.nr-pedido = ped-venda-ext.nr-pedido,
    EACH nota-fiscal FIELDS(dt-emis-nota vl-tot-nota nome-ab-cli dt-cancela nr-pedcli nat-operacao nr-nota-fis cod-emitente) NO-LOCK 
    WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
    AND   nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
    AND   nota-fiscal.dt-cancela  = ? :
    
    EXPORT DELIMITER ";" nota-fiscal.dt-emis-nota nota-fiscal.nr-nota-fis nota-fiscal.cod-emitente nota-fiscal.nat-operacao nota-fiscal.vl-tot-nota
                     nota-fiscal.vl-tot-nota * 0.08 .
    
    
END.
