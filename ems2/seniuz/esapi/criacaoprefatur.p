DEF VAR pedido AS CHAR.
DEF VAR c-embarque AS INT.
UPDATE pedido c-embarque.
FIND ped-venda WHERE nr-pedcli = pedido NO-LOCK NO-ERROR.
  CREATE pre-fatur.
  ASSIGN pre-fatur.cod-estabel  = ped-venda.cod-estabel
         pre-fatur.cod-priori   = ped-venda.cod-priori
         pre-fatur.dt-embarque  = TODAY
         pre-fatur.nat-operacao = ped-venda.nat-operacao
         pre-fatur.no-ab-reppri = ped-venda.no-ab-reppri
         pre-fatur.nome-abrev   = ped-venda.nome-abrev
         pre-fatur.nome-transp  = ped-venda.nome-transp
         pre-fatur.nr-pedcli    = ped-venda.nr-pedcli
         pre-fatur.nr-resumo    = 1
         pre-fatur.usuario      = "SUPER"
         pre-fatur.pais         = "Brasil"
         pre-fatur.cod-cond-pag = ped-venda.cod-cond-pag
         pre-fatur.cod-entrega  = ped-venda.cod-entrega
         pre-fatur.ind-sit-embarque = 1
         pre-fatur.nr-embarque  = c-embarque. 
FOR EACH bc-etiqueta WHERE bc-etiqueta.nr-pedcli = ped-venda.nr-pedcli.
    CREATE it-pre-fat.
    ASSIGN 
    it-pre-fat.it-codigo      =  bc-etiqueta.it-codigo
    it-pre-fat.cod-refer      =  bc-etiqueta.lote
    it-pre-fat.nome-abrev     =  bc-etiqueta.nome-abrev
    it-pre-fat.nr-embarque    =  bc-etiqueta.nr-embarque
    it-pre-fat.nr-pedcli      =  bc-etiqueta.nr-pedcli 
    it-pre-fat.nr-resumo      =  bc-etiqueta.nr-resumo
    it-pre-fat.nr-sequencia   =  bc-etiqueta.nr-seq
    it-pre-fat.qt-alocada     =  bc-etiqueta.qt-item
    it-pre-fat.qt-faturada    =  bc-etiqueta.qt-item.

    
END.





