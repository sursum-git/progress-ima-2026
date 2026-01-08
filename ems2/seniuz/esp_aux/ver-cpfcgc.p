/* ver-cpfcgc.p
** Verifica Cpf/Cgc dos emitente.
*/

def var i-list-fat1 as int extent 10 init [11,10,9,8,7,6,5,4,3,2].
def var i-list-fat2 as int extent 13 init [6,5,4,3,2,9,8,7,6,5,4,3,2].
def var i-digito    as int.
def var i-cont      as int.
def var i-soma      as int.
def var c-cgc1      as char format "x(19)" label "Calculado".
def var c-cgc2      as char format "x(19)" label "Informado".
def var l-erro      as log.

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Cliente;Razao-Social;Natureza;CNPJ;CNPJ-1;CNPJ-2" SKIP.

for each emitente no-lock: 
    assign l-erro = no.
    if emitente.natureza = 1 then do: /* F¡sica */
       if substr(emitente.cgc,4,1) = "." then
          assign c-cgc1 = substr(emitente.cgc,1,3) +
                          substr(emitente.cgc,5,3) +
                          substr(emitente.cgc,9,3)
                 c-cgc2 = c-cgc1 + substr(emitente.cgc,13,2).
       else
       if substr(emitente.cgc,10,1) = "." or
          substr(emitente.cgc,10,1) = "-" then
          assign c-cgc1 = substr(emitente.cgc,1,9)
                 c-cgc2 = substr(emitente.cgc,1,9) +
                          substr(emitente.cgc,11,2).
       else
          assign c-cgc1 = substr(emitente.cgc,1,9)
                 c-cgc2 = emitente.cgc.
       assign i-soma = 0.
       do i-cont = 2 to 10:
          assign i-soma = i-soma + (int(substr(c-cgc1,I-cont - 1,1)) *
                                        i-list-fat1[i-cont]).
       end.
       assign i-digito = 11 - (i-soma MODULO 11).
       if i-digito > 9 then
          assign i-digito = 0.
       assign c-cgc1 = c-cgc1 + string(i-digito,"9").
       assign i-soma = 0.
       do i-cont = 1 to 10:
          assign i-soma = i-soma + (int(substr(c-cgc1,I-cont,1)) *
                                    i-list-fat1[i-cont]).
       end.
       assign i-digito = 11 - (i-soma MODULO 11).
       if i-digito > 9 then
          assign i-digito = 0.
       assign c-cgc1 = c-cgc1 + string(i-digito,"9").
       if c-cgc1 <> c-cgc2 then
          assign l-erro = yes.
    end.
    else
    if emitente.natureza = 2 then do:
       if substr(emitente.cgc,3,1) = "." then
          assign c-cgc1 = substr(emitente.cgc,1,2) +
                          substr(emitente.cgc,4,3) +
                          substr(emitente.cgc,8,3) +
                          substr(emitente.cgc,12,4)
                 c-cgc2 = c-cgc1 + substr(emitente.cgc,17,2).
       else                                            
       if substr(emitente.cgc,13,1) = "." or
          substr(emitente.cgc,13,1) = "-" then
          assign c-cgc1 = substr(emitente.cgc,1,12)
                 c-cgc2 = substr(emitente.cgc,1,12) +
                          substr(emitente.cgc,14,2).
       else
          assign c-cgc1 = substr(emitente.cgc,1,12)
                 c-cgc2 = emitente.cgc.
       assign i-soma = 0.
       do i-cont = 2 to 13:
          assign i-soma = i-soma + (int(substr(c-cgc1,I-cont - 1,1)) *
                                        i-list-fat2[i-cont]).
       end.
       assign i-digito = 11 - (i-soma MODULO 11).
       if i-digito > 9 then
          assign i-digito = 0.
       assign c-cgc1 = c-cgc1 + string(i-digito,"9").
       assign i-soma = 0.
       do i-cont = 1 to 13:
          assign i-soma = i-soma + (int(substr(c-cgc1,I-cont,1)) *
                                    i-list-fat2[i-cont]).
       end.
       assign i-digito = 11 - (i-soma MODULO 11).
       if i-digito > 9 then
          assign i-digito = 0.
       assign c-cgc1 = c-cgc1 + string(i-digito,"9").
       if c-cgc1 <> c-cgc2 then
          assign l-erro = yes.
    end.
    else
    if emitente.cgc <> ? then
       assign l-erro = yes
              c-cgc1 = ""
              c-cgc2 = "".
       
    if l-erro then
       PUT emitente.cod-emitente ";"
           emitente.nome-emit ";"
           emitente.natureza ";"
           emitente.cgc ";"
           c-cgc1 ";"
           c-cgc2
           SKIP.
end.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.
