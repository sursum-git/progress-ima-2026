/* calc-dv.p
** Calcula Digito Verificador para codigos de Itens.
*/

def var i-list-fat  as int extent 13 init [6,5,4,3,2,9,8,7,6,5,4,3,2].
def var i-digito    as int format "9" label "DV".
def var i-cont      as int.
def var i-soma      as int.
DEF VAR c-aux       AS CHAR FORMAT "x(13)".

for each ref-item-ext where ref-item-ext.it-codigo = "502351" 
                        AND ref-item-ext.cod-refer = "0309010"
                      no-lock:
    ASSIGN c-aux  = ref-item-ext.it-codigo + ref-item-ext.cod-refer
           i-soma = 0.
    do i-cont = 1 to 13:
       if  substr(c-aux,1) >= "0" 
       and substr(c-aux,1) <= "9" then
           assign i-soma = i-soma + 
                      int(substr(c-aux,i-cont,1)) * i-list-fat[i-cont].
    end.
    assign i-digito = 11 - (i-soma MODULO 11).
    if i-digito > 9 then
       assign i-digito = 0.

    DISPLAY ref-item-ext.it-codigo
            ref-item-ext.cod-refer
            i-digito.
end.

