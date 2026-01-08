/******************************************************************
programa: esapi/enviarMail.p
objetivo: Enviar E-mail 
data:11/2025
Autor: Tadeu Silva Parreiras
*****************************************************************/
{utp/utapi019.i2}
DEFINE INPUT  PARAMETER pDestino      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRemetente    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pAsssunto     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pAnexo        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR tt-mensagem .

DEFINE VARIABLE cErro AS CHARACTER   NO-UNDO.

{esp/util.i}


RUN utp/utapi019.p PERSISTENT SET h-utapi019.

CREATE tt-envio2.
ASSIGN tt-envio2.versao-integracao = 1
       //tt-envio2.exchange          = TRUE
       //tt-envio2.servidor          = "servidor"
       //tt-envio2.porta             = 25         
       tt-envio2.destino           = pDestino
       tt-envio2.remetente         = pRemetente
       tt-envio2.assunto           = pAsssunto
       tt-envio2.arq-anexo         = pAnexo.   
 
CREATE tt-paramEmail2.
ASSIGN tt-paramEmail2.caminhoEmail = 5
       //tt-paramEmail2.mailUser     = 'email.precon'
       //tt-paramEmail2.mailpass     = 'Precon2K10'
       //tt-paramEmail2.ssl          = NO
       //tt-paramEmail2.tls          = NO
       //tt-paramEmail2.DEBUG        = YES
       .

RUN pi-execute4 in h-utapi019(INPUT  TABLE tt-envio2,
                              INPUT  TABLE tt-mensagem,
                              INPUT  TABLE tt-paramEmail2,
                              INPUT  TABLE ttAttachment,
                              OUTPUT TABLE tt-erros).

IF RETURN-VALUE = "NOK" THEN DO:
    
   FOR EACH tt-erros:
       RUN incrValor(INPUT-OUTPUT cErro, 
                    string(tt-erros.cod-erro) + tt-erros.desc-erro,
                    CHR(13)
                    ). 
   END.
   UNDO, THROW NEW PROGRESS.lang.appError(cErro,995).

END.
