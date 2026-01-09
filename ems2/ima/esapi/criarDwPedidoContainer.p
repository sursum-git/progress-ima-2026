/**********************************************************************
 programa:esapi/criarDwPedidoContainer.p
 objetivo:Percorre os Pedidos em Aberto e grava as informaá‰es
 dos mesmos nas diversas tabelas necessarias
 Autor:Tadeu Silva
 Data: 01/2026
***********************************************************************/

FOR EACH ped-venda NO-LOCK:

    IF ped-venda.cod-sit-ped = 3 THEN DO: //atendido total
    
     //se jah existe o pedido como atendido total, n∆o continua a criar hist¢rico
      IF CAN-FIND(FIRST dw_venda_container
        WHERE dw_venda_container.cod_estabel    = ped-venda.cod-estabel
        AND   dw_venda_container.nr_pedido      =  ped-venda.nr-pedido
        AND   dw_venda_container.cod_sit_ped    = 3 
      ) THEN NEXT.       
       
    END.
    
    CREATE dw_venda_container.
    ASSIGN dw_venda_container.id = NEXT-VALUE(seq_dw_venda_container).
    
END.





{esp/lancarErros.i}


/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 id                               int6        i
   11 cod_estabel                      char        i
   12 nr_pedido                        inte        i
   20 dt_pedido                        date
   30 cliente_id                       inte
   40 cliente_triang_id                inte
   50 prazo_medio                      inte
   60 tipo_pedido                      char
   70 tb_preco_id                      inte
   80 tipo_frete                       char
   90 repres_id                        inte
  100 nr_container                     inte        i
  110 cod_sit_pedido                   inte
  120 dw_trans_id                      inte        i
  130 vl_pedido                        deci-2
*/

