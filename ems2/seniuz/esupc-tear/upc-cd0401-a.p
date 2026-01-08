/* Leave do CEP */

DEFINE NEW GLOBAL SHARED VAR h-cep       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco  AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro    AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade    AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado    AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-natureza  AS HANDLE.

DEF VAR c-situacao      AS CHAR.
DEF var c-endereco      LIKE emitente.endereco.
DEF VAR c-bairro        LIKE emitente.bairro.  
DEF VAR c-cidade        LIKE emitente.cidade.  
DEF VAR c-estado        LIKE emitente.estado. 

IF h-natureza:SCREEN-VALUE <> "Estrangeiro" AND
   KEYFUNCTION(LASTKEY) <> 'BACK-TAB' THEN DO.
   RUN esapi/seek-cep.p (INPUT h-cep:SCREEN-VALUE,
                         OUTPUT c-situacao,
                         OUTPUT c-endereco,
                         OUTPUT c-bairro,
                         OUTPUT c-cidade,
                         OUTPUT c-estado).
    
   IF c-situacao = '0' THEN DO.
      MESSAGE 'Cep ' h-cep:SCREEN-VALUE ' n∆o Encontrado no WebService do Correio...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      /*
      APPLY 'entry' TO h-cep.
      RETURN NO-APPLY.
      */
   END.
    
   IF h-endereco:SCREEN-VALUE = "" THEN
      ASSIGN h-endereco:SCREEN-VALUE = c-endereco
             h-bairro:SCREEN-VALUE   = c-bairro
             h-cidade:SCREEN-VALUE   = c-cidade
             h-estado:SCREEN-VALUE   = c-estado.
    
   IF NOT c-endereco MATCHES "*" + h-endereco:SCREEN-VALUE + "*" THEN DO.
      MESSAGE "Endereáo Informado difere do Endereáo do CEP," SKIP
              "Deseja Atualizar ?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-atu-cep AS LOG.
      IF l-atu-cep THEN
         ASSIGN h-endereco:SCREEN-VALUE = REPLACE(h-endereco:SCREEN-VALUE,ENTRY(1,h-endereco:SCREEN-VALUE),c-endereco)
                h-bairro:SCREEN-VALUE   = c-bairro
                h-cidade:SCREEN-VALUE   = c-cidade
                h-estado:SCREEN-VALUE   = c-estado.
   END.
END.
