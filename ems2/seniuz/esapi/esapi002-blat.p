/****************************************************************************
** Programa: esapi001.p - API de envio de e-mail
** Autor   : Antonio Souza (toninho)
** Data    : 13/01/2005
** Objetivo: Chamar a API padr∆o do EMS de envio de e-mail
*****************************************************************************/
DEFINE INPUT  PARAMETER c-remetente   AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE INPUT  PARAMETER c-destino     AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE INPUT  PARAMETER c-assunto     AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE INPUT  PARAMETER c-mensagem    AS CHARACTER FORMAT "X(15000)" NO-UNDO.
DEFINE INPUT  PARAMETER c-arq-anexto  AS CHARACTER FORMAT "x(40)" NO-UNDO.  
DEFINE INPUT  PARAMETER l-mostra-erro AS LOGICAL   NO-UNDO.

DEFINE VARIABLE c-cod-servid AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-porta  AS INTEGER    NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.


{utp/ut-glob.i}

OUTPUT TO p:\LOGemail.txt.
    PUT UNFORMATTED
        "usuario:" c-seg-usuario SKIP
        "Remitente:" c-remetente SKIP
        "Destino:"   c-destino SKIP
        "Assunto:"   c-assunto SKIP
        "Mensagem:"  c-mensagem SKIP
        "MOstra erro:" l-mostra-erro SKIP
        "Horario:" string(NOW,"99/99/9999 hh:mm:ss") SKIP.
OUTPUT CLOSE.

{utp/utapi019.i}

FIND FIRST param_email NO-LOCK NO-ERROR.
ASSIGN c-cod-servid = IF AVAIL param_email THEN param_email.cod_servid_e_mail ELSE "" 
       i-num-porta = IF AVAIL param_email THEN param_email.num_porta ELSE 0.

FOR EACH tt-erros:
    DELETE tt-erros.
END.

FOR EACH tt-mensagem:
    DELETE tt-mensagem.
END.

FOR EACH tt-envio2:
    DELETE tt-envio2.
END.

CREATE tt-mensagem.
ASSIGN tt-mensagem.seq-mensagem = 1
       tt-mensagem.mensagem     = c-mensagem.

CREATE tt-envio2.
ASSIGN tt-envio2.versao-integracao = 1
       tt-envio2.exchange          = NO
       tt-envio2.servidor          = c-cod-servid
       tt-envio2.porta             = i-num-porta
       tt-envio2.remetente         = c-remetente
       tt-envio2.arq-anexo         = c-arq-anexto   
       tt-envio2.destino           = c-destino + 
                                     " -u "  + TRIM(param_email.cod_usuar_email) + 
                                     " -pw " + TRIM(param_email.cod_senha) 
       tt-envio2.assunto           = c-assunto.

IF c-mensagem MATCHES "*<html*" THEN 
   ASSIGN tt-envio2.formato = "HTML".

RUN utp/utapi019.p PERSISTENT SET h-utapi019.

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "envemail.txt").       
RUN pi-execute2 in h-utapi019 (INPUT  TABLE tt-envio2,
                               INPUT  TABLE tt-mensagem,
        	                   OUTPUT TABLE tt-erros).
OUTPUT CLOSE.

DELETE PROCEDURE h-utapi019.

IF CAN-FIND(FIRST tt-erros) THEN DO:
   IF l-mostra-erro THEN DO:
      c-mensagem = "N∆o foi poss°vel enviar o e-mail solicitado com assunto: " +
                   c-assunto + 
                   ". Problemas ocorridos: " + CHR(10) + CHR(10).

      FOR EACH tt-erros:
          ASSIGN c-mensagem = c-mensagem + 
                              STRING(tt-erros.cod-erro) + "-" + 
                              tt-erros.desc-erro + CHR(10).
      END.
      MESSAGE "E-mail n∆o enviado" SKIP
          c-mensagem
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      /*RUN utp/ut-msgs.p (INPUT 2,
                        INPUT "E-mail n∆o enviado!",
                        INPUT c-mensagem).*/
   END.
   RETURN "NO".
END.
ELSE RETURN "YES".
