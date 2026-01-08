/* Programa: imp-avanco.p
** Objetivo: Importar cadastro de produtos da RM para Avan‡o. 
*/

DEF TEMP-TABLE tt-produto
    FIELD codigo        AS INT FORMAT "9999999999999"   
    FIELD descricao     AS CHAR FORMAT "x(35)"
    FIELD unidade       AS CHAR FORMAT "x(3)"
    FIELD desc-res      AS CHAR FORMAT "x(20)"
    FIELD livre         AS CHAR FORMAT "x(1)"
    FIELD tribut        AS INT FORMAT "9"
    FIELD aliq-ori      AS INT FORMAT "9999"
    FIELD aliq-efet     AS INT FORMAT "9999"
    FIELD reducao       AS INT FORMAT "999999"
    FIELD pis-cofins    AS CHAR FORMAT "x(1)"
    FIELD fracionado    AS CHAR FORMAT "x(1)"
    FIELD composto      AS CHAR FORMAT "x(1)"
    FIELD projeto       AS INT FORMAT "99"
    FIELD acao-proj     AS INT FORMAT "99"
    FIELD livre1        AS CHAR FORMAT "x(1)"
    FIELD preco-venda   AS INT FORMAT "9999999999"
    FIELD peso-var      AS CHAR FORMAT "x(1)"
    FIELD setor         AS INT FORMAT "99" 
    FIELD grupo         AS INT FORMAT "999" 
    FIELD cod-altern    AS CHAR FORMAT "x(13)"
    FIELD desc-max      AS INT FORMAT "99999"
    FIELD livre2        AS CHAR FORMAT "x(2)"
    FIELD concentr      AS INT FORMAT "9999999999999"
    FIELD fator         AS INT FORMAT "9999"
    FIELD marca         AS CHAR FORMAT "x(10)"
    FIELD quant-max     AS INT FORMAT "999999"
    FIELD desc-expand   AS CHAR FORMAT "x(75)".

    
input from "c:/lixo/produto.csv".
SET ^.

repeat:
   create tt-produto.
   import delimiter ";" tt-produto.
end.
input close.

OUTPUT TO "c:/lixo/produto.txt".
FOR EACH tt-produto:
    IF tt-produto.codigo = 0 THEN NEXT.
    /*
    DISP tt-produto WITH SIDE-LABELS 1 COLUMN WIDTH 128.
    */
    PUT tt-produto.codigo     
        tt-produto.descricao  
        tt-produto.unidade    
        tt-produto.desc-res   
        tt-produto.livre      
        tt-produto.tribut     
        tt-produto.aliq-ori   
        tt-produto.aliq-efet  
        tt-produto.reducao    
        tt-produto.pis-cofins 
        tt-produto.fracionado 
        tt-produto.composto   
        tt-produto.projeto    
        tt-produto.acao-proj  
        tt-produto.livre1     
        tt-produto.preco-venda
        tt-produto.peso-var   
        tt-produto.setor      
        tt-produto.grupo      
        tt-produto.cod-altern 
        tt-produto.desc-max   
        tt-produto.livre2     
        tt-produto.concentr   
        tt-produto.fator      
        tt-produto.marca      
        tt-produto.quant-max  
        tt-produto.desc-expand
        SKIP.
END.
OUTPUT CLOSE.
