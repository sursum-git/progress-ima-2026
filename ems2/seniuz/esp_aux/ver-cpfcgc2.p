/* ver-cpfcgc2.p
** Verifica Cpf/Cgc dos emitentes e coloca mascara para os corretos.
*/

def buffer b-emitente for emitente.

def var l-atualizar as log format "Sim/Nao".
def var i-list-fat1 as int extent 10 init [11,10,9,8,7,6,5,4,3,2].
def var i-list-fat2 as int extent 13 init [6,5,4,3,2,9,8,7,6,5,4,3,2].
def var i-digito    as int.
def var i-cont      as int.
def var i-soma      as int.
def var c-cgc       like emitente.cgc format "99.999.999/9999-99".
def var c-cpf       like emitente.cgc format "999.999.999-99".
def var c-cgc-novo  like emitente.cgc.
def var c-cpf-novo  like emitente.cgc.
def var c-cgc1      as char format "x(19)" label "Calculado".
def var c-cgc2      as char format "x(19)" label "Informado".
def var l-erro      as log.

message "Atualizar CGC/CPF ? (S/N):" update l-atualizar.

for each emitente where emitente.cgc <> ?: 
    assign l-erro = no.
    if emitente.natureza = "F" then do:
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
    if emitente.natureza = "J" then do:
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
       
    if not l-erro then
       if  index(emitente.cgc,".",1) = 0 
       and index(emitente.cgc,"/",1) = 0
       and index(emitente.cgc,"-",1) = 0 then do:
           if emitente.natureza = "J" then do:
              assign c-cgc = emitente.cgc.
              assign c-cgc-novo = substr(c-cgc,1,2) + "." + 
                                  substr(c-cgc,3,3) + "." +
                                  substr(c-cgc,6,3) + "/" +
                                  substr(c-cgc,9,4) + "-" +
                                  substr(c-cgc,13,2).
              find b-emitente where b-emitente.cgc       = c-cgc-novo
                                and b-emitente.identific = emitente.identific
                                    no-lock no-error.
              if  l-atualizar
              and not avail b-emitente then
                  assign emitente.cgc = c-cgc-novo.
           end.
           else do:
              assign c-cpf = emitente.cgc.
              assign c-cpf-novo = substr(c-cpf,1,3) + "." + 
                                  substr(c-cpf,4,3) + "." +
                                  substr(c-cpf,7,3) + "-" +
                                  substr(c-cpf,10,2).
              find b-emitente where b-emitente.cgc       = c-cpf-novo
                                and b-emitente.identific = emitente.identific
                                    no-lock no-error.
              if  l-atualizar
              and not avail b-emitente then
                  assign emitente.cgc = c-cpf-novo.
           end.
           if not l-atualizar then
              display emitente.cod-emit
                      emitente.cgc
                      emitente.identific
                      emitente.natureza
                      c-cgc
                      c-cpf
                      avail b-emitente.
           assign c-cgc = ""
                  c-cpf = "".
       end.
end.