DEFINE STREAM s1.
DEFINE STREAM s2.
DEFINE STREAM s3.
OUTPUT  STREAM s1 TO c:\temp\s1.txt.
OUTPUT  STREAM s2 TO c:\temp\s2.txt.
OUTPUT  STREAM s3 TO c:\temp\s3.txt.
FOR EACH wp
WHERE login = 'claudio alve' OR login = 'benivaldo'  BY data_hora:
    FOR EACH wp_estoque_preco
        WHERE wp_estoque_preco.cod_wp = wp.cod_wp.
        EXPORT STREAM s1 DELIMITER  "|" 
               cod_estabel                     
               it_codigo                       
               cod_refer                       
               qt_pedido                       
               qt_saldo                        
               qt_disponivel                   
               qt_reservada                    
               moeda                           
               preco_prazo01                   
               preco_prazo02                   
               preco_prazo03                   
               preco_prazo04                   
               preco_prazo05                   
               preco_promocao                  
               qt_programada .

    END.
    FOR EACH wp_ped_venda WHERE
         wp_ped_venda.cod_wp = wp.cod_wp:
         EXPORT STREAM s2 DELIMITER "|" 
             int64(COD_wp) FORMAT "99999999999999" 
             login      
             data_hora  
             substr(pagina,1,100) FORMAT 'x(100)'
             cod_estabel             
              nr_container            
              nome_abrev              
              nr_pedcli               
              dt_implant              
              cod_sit_ped             
              tp_pedido               
              no_ab_reppri            
              cidade                  
              estado                  
              cidade_cif              
              cod_sit_aval            
              cod_sit_preco           
              vl_liquido              
              vl_desconto             
              vl_bruto                
              cod_emitente            
              preposto                
              origem                  
              completo                
              cod_priori              
              mo_codigo.

    END.
    FOR EACH wp_nota_fiscal
        WHERE wp_nota_fiscal.cod_wp = wp.cod_wp.
        EXPORT STREAM s3 DELIMITER "|" 
            int64(COD_wp) FORMAT "99999999999999" 
             login      
             data_hora  
             substr(pagina,1,100) FORMAT 'x(100)'
             tipo_nota                    
             nr_nota_fis                 
             serie                       
             cod_emitente                
             no_ab_reppri                
             cod_repres                  
             dt_emis_nota                
             vl_nota                     
             nr_nota_fis_original        
             cod_wp                      
             nr_pedcli                   
             vl_bruto                    
             vl_desconto                 
             preposto                    
             cidade                      
             estado.

    END.
END.

OUTPUT STREAM s1 CLOSE.
OUTPUT STREAM s2 CLOSE.
OUTPUT STREAM s3 CLOSE.
/*
cod_wp                           >>>>>>>>>>>>>>>>>>>999
login                            X(20)
data_hora                        99/99/9999 HH:MM:SS
pagina                           x(100)
filtro                           x(2000)
tabela                           x(50)
*/

/*
nota fiscal

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 tipo_nota                        inte        i
   20 nr_nota_fis                      char        i
   30 serie                            char        i
   40 cod_emitente                     inte        i
   50 no_ab_reppri                     char
   60 cod_repres                       inte
   70 dt_emis_nota                     date        i
   80 vl_nota                          deci-2
   90 nr_nota_fis_original             inte
  100 cod_wp                           char        i
  110 nr_pedcli                        char
  120 vl_bruto                         deci-2
  130 vl_desconto                      deci-2
  140 preposto                         char
  150 cidade                           char
  160 estado                           char


   estoque 
   10 cod_wp                           char
   20 cod_estabel                      char
   30 it_codigo                        char
   40 cod_refer                        char
   50 qt_pedido                        deci-2
   60 qt_saldo                         deci-2
   70 qt_disponivel                    deci-2
   80 qt_reservada                     deci-2
   90 moeda                            inte
  100 preco_prazo01                    deci-2
  110 preco_prazo02                    deci-2
  120 preco_prazo03                    deci-2
  130 preco_prazo04                    deci-2
  140 preco_prazo05                    deci-2
  150 preco_promocao                   deci-2
  160 qt_programada                    deci-2

ped-venda  
   10 cod_estabel                      char        im
   20 nr_container                     inte
   30 nome_abrev                       char        im
   40 nr_pedcli                        char        im
   50 dt_implant                       date        i
   60 cod_sit_ped                      inte        i
   70 tp_pedido                        char        i
  170 no_ab_reppri                     char        im
  180 cidade                           char
  190 estado                           char
  200 cidade_cif                       char
  210 cod_sit_aval                     inte
  220 cod_sit_preco                    inte
  230 vl_liquido                       deci-2
  240 vl_desconto                      deci-2
  250 vl_bruto                         deci-2
  270 cod_emitente                     inte
  280 preposto                         char
  290 cod_wp                           char        i
  300 origem                           inte
  310 completo                         inte
  320 cod_priori                       inte
  330 mo_codigo                        inte


*/

