DEFINE INPUT PARAMETER p-cep       AS HANDLE.
DEFINE INPUT PARAMETER p-endereco  AS HANDLE.
DEFINE INPUT PARAMETER p-bairro    AS HANDLE.
DEFINE INPUT PARAMETER p-cidade    AS HANDLE.
DEFINE INPUT PARAMETER p-estado    AS HANDLE.
DEFINE INPUT PARAMETER p-pais      AS HANDLE.

DEF VAR c-situacao      AS CHAR.
DEF var c-endereco      LIKE emitente.endereco.
DEF VAR c-bairro        LIKE emitente.bairro.  
DEF VAR c-cidade        LIKE emitente.cidade.  
DEF VAR c-estado        LIKE emitente.estado. 

IF KEYFUNCTION(LASTKEY) <> 'BACK-TAB' THEN DO.
   RUN CepOnline/Api/seek-cep.p (INPUT p-cep:SCREEN-VALUE,
                                 OUTPUT c-situacao,
                                 OUTPUT c-endereco,
                                 OUTPUT c-bairro,
                                 OUTPUT c-cidade,
                                 OUTPUT c-estado).
   
   IF c-situacao = '0' THEN DO.
      MESSAGE 'Cep ' p-cep:SCREEN-VALUE ' n∆o Encontrado no WebService do Correio...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      /*
      APPLY 'entry' TO p-cep.
      RETURN NO-APPLY.
      */
   END.

   IF p-endereco:SCREEN-VALUE = "" THEN
      ASSIGN p-endereco:SCREEN-VALUE = c-endereco
             p-bairro:SCREEN-VALUE   = c-bairro
             p-cidade:SCREEN-VALUE   = c-cidade
             p-estado:SCREEN-VALUE   = c-estado
             p-pais:SCREEN-VALUE     = "BRASIL".
    
   IF NOT c-endereco MATCHES "*" + p-endereco:SCREEN-VALUE + "*" THEN DO.
      MESSAGE "Endereáo Informado difere do Endereáo do CEP," SKIP
              "Deseja Atualizar ?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-atu-cep AS LOG.
      IF l-atu-cep THEN
         ASSIGN p-endereco:SCREEN-VALUE = REPLACE(p-endereco:SCREEN-VALUE,ENTRY(1,p-endereco:SCREEN-VALUE),c-endereco)
                p-bairro:SCREEN-VALUE   = c-bairro
                p-cidade:SCREEN-VALUE   = c-cidade
                p-estado:SCREEN-VALUE   = c-estado
                p-pais:SCREEN-VALUE     = "BRASIL".
   END.
END.
