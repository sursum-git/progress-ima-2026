DEF VAR c-prog-balanca AS CHAR.
DEF VAR c-peso-balanca AS CHAR.
DEF VAR c-comando      AS CHAR.
DEF VAR c-peso         AS CHAR FORMAT "x(10)".
DEF VAR de-peso-lido   AS DEC.
DEF VAR l-capturar     AS LOG.
DEF VAR i-tempo-ini    AS INT.

def var h-acomp as handle no-undo.

FIND mp-param NO-LOCK.

ASSIGN c-prog-balanca = mp-param.dir-balanca + "\balanca.exe"
       c-peso-balanca = mp-param.dir-balanca + "\peso.txt"
       c-comando      = c-prog-balanca + " " + c-peso-balanca.

UPDATE l-capturar.

IF SEARCH(c-prog-balanca) <> ? THEN DO:

   IF l-capturar THEN do:
      RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
      {utp/ut-liter.i Capturando_Peso *}
      RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

      RUN pi-acompanhar IN h-acomp (INPUT "Aguarde...").

      OS-DELETE SILENT VALUE(c-peso-balanca).
      ASSIGN i-tempo-ini = TIME.
      DO WHILE (TIME - i-tempo-ini) < 30 AND SEARCH(c-peso-balanca) = ?:
         OS-COMMAND SILENT VALUE(c-comando).
      END.
      RUN pi-finalizar in h-acomp.
   END.

   IF SEARCH(c-peso-balanca) <> ? THEN DO:
      INPUT FROM value(c-peso-balanca) NO-ECHO.
      REPEAT:
         SET c-peso.
      END.
      INPUT CLOSE.

      IF INT(SUBSTR(c-peso,6,5)) <> 0 THEN
         ASSIGN de-peso-lido = INT(SUBSTR(c-peso,6,5)).
      ELSE
         ASSIGN de-peso-lido = 0.
   END.
   ELSE
       ASSIGN de-peso-lido = 0.
END.
ELSE 
   MESSAGE "Programa de captura de peso n∆o foi encontrado."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

DISP c-prog-balanca FORMAT "x(23)"
     c-peso-balanca FORMAT "x(19)"
     c-comando      FORMAT "x(50)"
     c-peso         
     de-peso-lido
     WITH SIDE-LABELS 1 COLUMN.


