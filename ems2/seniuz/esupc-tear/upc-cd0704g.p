/* Programa: upc-cd0704g.p
** Objetivo: Verificar se para emitente existe outro com o mesmo e-mail.
** Autor...: Gilvando Souza Araujo - Novembro/2013.
*/

DEF NEW GLOBAL SHARED VAR h-e-mail AS HANDLE.

FIND LAST emitente WHERE 
          emitente.e-mail MATCHES "*" + TRIM(h-e-mail:SCREEN-VALUE) + "*"
          NO-LOCK NO-ERROR.

IF AVAIL emitente THEN
   MESSAGE "Encontrei um cliente com este e-mail" SKIP(1)
           "Cliente: " + STRING(emitente.cod-emitente) + " - " + emitente.nome-abrev SKIP
           "Cidade.: " + TRIM(emitente.cidade) + "/" + emitente.estado 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
