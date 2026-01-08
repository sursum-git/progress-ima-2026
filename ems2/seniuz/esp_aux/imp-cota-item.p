/* Programa: imp-cota-item.p
** Objetivo: Importar o arquivo "cota-item.txt" com dados parciais das contas por item.
*/

def temp-table tt-cota-item
    FIELD it-codigo    LIKE cota-item.it-codigo    
    FIELD item-adic1   LIKE cota-item.item-adic1   
    FIELD item-adic2   LIKE cota-item.item-adic2   
    FIELD item-adic3   LIKE cota-item.item-adic3   
    FIELD item-adic4   LIKE cota-item.item-adic4   
    FIELD item-adic5   LIKE cota-item.item-adic5   
    FIELD item-adic6   LIKE cota-item.item-adic6   
    FIELD estoque      LIKE cota-item.estoque      
    FIELD producao     LIKE cota-item.producao     
    FIELD cart-mes-ant LIKE cota-item.cart-mes-ant  
    FIELD cota1        LIKE cota-item.cota1         
    FIELD susp-cota1   LIKE cota-item.susp-cota1     
    FIELD cota2        LIKE cota-item.cota2         
    FIELD susp-cota2   LIKE cota-item.susp-cota2    
    FIELD dt-min-ped   LIKE cota-item.dt-min-ped.

input from "c:/temp/cota-item.txt".
SET ^.

repeat:
   create tt-cota-item.
   import delimiter ";" tt-cota-item.
end.
input close.

FOR EACH tt-cota-item:
    IF tt-cota-item.it-codigo = "" THEN NEXT.
    /*
    DISP tt-cota-item WITH SIDE-LABELS 1 COLUMN.
    */
    CREATE cota-item.
    ASSIGN cota-item.it-codigo    = tt-cota-item.it-codigo   
           cota-item.item-adic1   = tt-cota-item.item-adic1  
           cota-item.item-adic2   = tt-cota-item.item-adic2   
           cota-item.item-adic3   = tt-cota-item.item-adic3  
           cota-item.item-adic4   = tt-cota-item.item-adic4  
           cota-item.item-adic5   = tt-cota-item.item-adic5  
           cota-item.item-adic6   = tt-cota-item.item-adic6  
           cota-item.estoque      = tt-cota-item.estoque     
           cota-item.producao     = tt-cota-item.producao    
           cota-item.cart-mes-ant = tt-cota-item.cart-mes-ant
           cota-item.cota1        = tt-cota-item.cota1       
           cota-item.susp-cota1   = tt-cota-item.susp-cota1  
           cota-item.cota2        = tt-cota-item.cota2       
           cota-item.susp-cota2   = tt-cota-item.susp-cota2  
           cota-item.dt-min-ped   = tt-cota-item.dt-min-ped.
END.
