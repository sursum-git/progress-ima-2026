/*Enviando E-MAIL, utilizando Blat:*/

{utp/utapi019.i}
run utp/utapi019.p persistent set h-utapi019.
create tt-envio2.
assign tt-envio2.versao-integracao = 1
       tt-envio2.servidor    = "smtp.imatextil.com.br"
       tt-envio2.porta       = 587
       tt-envio2.destino     = "eduardo.magno@imatextil.com.br"
       tt-envio2.remetente   = "imatextil@imatextil.com.br"
       tt-envio2.assunto     = "teste"
       tt-envio2.arq-anexo   = "c:/tmp/texto.doc"
       tt-envio2.formato     = "HTML".

create tt-mensagem.
assign tt-mensagem.seq-mensagem = 1
       tt-mensagem.mensagem     = "<h1><center>message body 1</pre>".

create tt-mensagem.
assign tt-mensagem.seq-mensagem = 2
       tt-mensagem.mensagem     = "<h1><center>message body 2</pre>".

output to value(session:temp-directory + "envemail.txt"). 
    run pi-execute2 in h-utapi019 (input  table tt-envio2,
                                   input  table tt-mensagem,
                                   output table tt-erros).
output close.
       
if  return-value = "NOK" then do:
    for each tt-erros:
        disp tt-erros with 1 column width 300.
    end. 
end.
delete procedure h-utapi019.
