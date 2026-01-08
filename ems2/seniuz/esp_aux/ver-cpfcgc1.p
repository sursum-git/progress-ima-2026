/* ver-cpfcgc1.p
** Verifica Cpf/Cgc em duplicidade dos emitentes.
*/

def var c-cgc  as char format "x(19)" label "CPF/CGC".
def var l-erro as log.

def buffer b-emitente for emitente.

output to "spool/ver-cpfcgc1.lst" paged page-size 64.
for each emitente no-lock: 
    assign l-erro = no.
    if emitente.natureza = "F" then do:
       if substr(emitente.cgc,4,1) = "." then
          assign c-cgc = substr(emitente.cgc,1,3) +
                         substr(emitente.cgc,5,3) +
                         substr(emitente.cgc,9,3) +
                         substr(emitente.cgc,13,2).
       else
       if substr(emitente.cgc,10,1) = "." or
          substr(emitente.cgc,10,1) = "-" then
          assign c-cgc = substr(emitente.cgc,1,9) +
                         substr(emitente.cgc,11,2).
       else
          assign c-cgc = emitente.cgc.

       find first b-emitente 
            where b-emitente.cgc          =  c-cgc
              and b-emitente.cod-emitente <> emitente.cod-emitente
                  no-lock no-error.
       if avail b-emitente then
          assign l-erro = yes.
    end.
    else
    if emitente.natureza = "J" then do:
       if substr(emitente.cgc,3,1) = "." then
          assign c-cgc = substr(emitente.cgc,1,2) +
                         substr(emitente.cgc,4,3) +
                         substr(emitente.cgc,8,3) +
                         substr(emitente.cgc,12,4) +
                         substr(emitente.cgc,17,2).
       else                                            
       if substr(emitente.cgc,13,1) = "." or
          substr(emitente.cgc,13,1) = "-" then
          assign c-cgc = substr(emitente.cgc,1,12) +
                         substr(emitente.cgc,14,2).
       else
          assign c-cgc = emitente.cgc.

       find first b-emitente 
            where b-emitente.cgc          =  c-cgc
              and b-emitente.cod-emitente <> emitente.cod-emitente
                  no-lock no-error.
       if avail b-emitente then
          assign l-erro = yes.
    end.
       
    if l-erro then do:
       display emitente.cod-emitente
               emitente.nome-emit
               emitente.identific
               emitente.natureza
               emitente.cgc
               skip
               b-emitente.cod-emitente
               b-emitente.nome-emit
               b-emitente.identific
               b-emitente.natureza
               b-emitente.cgc
               with width 132.
       put "---" skip.        
    end.
end.
output close.
