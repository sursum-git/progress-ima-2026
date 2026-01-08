FIND FIRST ems5.cliente
    WHERE cdn_cliente = 31361.
/*UPDATE cliente WITH 1 COL WIDTH 550.*/

FOR EACH tit_acr 
    WHERE tit_acr.cdn_cliente =  ems5.cliente.cdn_cliente :
    DISP tit_acr WITH 1 COL WIDTH 550.
    IF tit_acr.num_pessoa <>  cliente.num_pessoa THEN DO:
       ASSIGN tit_acr.num_pessoa = cliente.num_pessoa.
    END.
END.
