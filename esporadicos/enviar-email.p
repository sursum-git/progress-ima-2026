{utp/utapi019.i2}
CREATE tt-mensagem.
ASSIGN tt-mensagem.seq-mensagem =  10
       tt-mensagem.mensagem     = 'TESTE'.
       
       
RUN esapi/enviarEmail.p('tadeu.parreiras@gmail.com',                           
                        'pedidos@imatextil.com.br',
                        'TESTE',
                        '',
                        TABLE tt-mensagem).       
                        
                        
{esp/retornarErrosMSG.i}                        
