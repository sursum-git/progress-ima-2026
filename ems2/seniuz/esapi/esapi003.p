/****************************************************************************
** Programa: esapi001.p - API de envio de e-mail
** Autor   : Antonio Souza (toninho)
** Data    : 13/01/2005
** Objetivo: Chamar a API padr∆o do EMS de envio de e-mail
*****************************************************************************/
{utp/ut-glob.i}
{utp/utapi019.i2}


DEFINE INPUT  PARAMETER c-remetente   AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE INPUT  PARAMETER c-destino     AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE INPUT  PARAMETER c-assunto     AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR tt-mensagem.
DEFINE INPUT  PARAMETER c-arq-anexo   AS CHARACTER FORMAT "x(40)" NO-UNDO.  
DEFINE INPUT  PARAMETER l-mostra-erro AS LOGICAL   NO-UNDO.

DEFINE VARIABLE c-cod-servid AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-num-porta  AS INTEGER    NO-UNDO.

DEF VAR c-mensagem AS CHAR.
DEF VAR i-ct AS INT.

FIND FIRST param_email NO-LOCK NO-ERROR.
ASSIGN c-cod-servid = IF AVAIL param_email THEN param_email.cod_servid_e_mail ELSE "" 
       i-num-porta = IF AVAIL param_email THEN param_email.num_porta ELSE 0.

FOR EACH tt-erros:
    DELETE tt-erros.
END.

FOR EACH tt-envio2:
    DELETE tt-envio2.
END.

CREATE tt-envio2.
ASSIGN tt-envio2.versao-integracao = 1
       tt-envio2.exchange          = NO
       tt-envio2.servidor          = c-cod-servid
       tt-envio2.porta             = i-num-porta
       tt-envio2.remetente         = c-remetente
       tt-envio2.arq-anexo         = c-arq-anexo    
       tt-envio2.destino           = c-destino  
       tt-envio2.assunto           = c-assunto
       tt-envio2.formato           = "HTML".

CREATE tt-paramEmail2.
ASSIGN tt-paramEmail2.caminhoEmail = 5  // 0-Unix  1-Blat  2-Exchange  3-Java  4-MailSend(SSL)  5-Datasul Mail Service
                                        // Caso for atribu°do um valor diferente desses, o valor padr∆o ser† BLAT
       tt-paramEmail2.mailUser = TRIM(param_email.cod_usuar_email)  // C¢digo do usu†rio para autenticaá∆o (se necess†rio)
       tt-paramEmail2.mailPass = TRIM(param_email.cod_senha)      // Senha do usu†rio para autenticaá∆o (se necess†rio)
       tt-paramEmail2.TLS = YES // - Apenas para Datasul Mail Service
       tt-paramEmail2.SSL = NO  // - Apenas para Datasul Mail Service
       tt-paramEmail2.Debug	= NO   // Ativa o debug para o servidor de aplicaá∆o
       //tt-paramEmail2.remetente = NO //	logical	no	Remetente do envio de e-mail
       tt-paramEmail2.ativaRemetPadrao = NO // Ativa o comportamento "Responder Para" (*)
       tt-paramEmail2.codRemetPadrao = ''. 	//C¢digo (e-mail) do remetente que ira receber a resposta, caso o campo logico ativaRemetPadrao estiver como yes (*) (aviso)  Os parÉm

DO i-ct = 1 TO NUM-ENTRIES(c-arq-anexo).
   CREATE ttAttachment.
   ASSIGN ttAttachment.FILENAME = ENTRY(i-ct,c-arq-anexo).
          //ttAttachment.fileType = ''.

   COPY-LOB FILE ttAttachment.FILENAME TO ttAttachment.fileContent.
END.

RUN utp/utapi019.p PERSISTENT SET h-utapi019.

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "envemail.txt").       
RUN pi-execute4 in h-utapi019(INPUT  TABLE tt-envio2,
                              INPUT  TABLE tt-mensagem,
                              INPUT  TABLE tt-paramEmail2,
                              INPUT  TABLE ttAttachment,
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
