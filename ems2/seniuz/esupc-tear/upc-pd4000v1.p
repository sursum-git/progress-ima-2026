/* Programa: upc-pd4000V1.p
** Objetivo: Trigger de 'Value-Changed' para o campo fi-acond (Acondicionamento)
**           Verificar se o Acondiconamento est  sendo digitado corretamente
** Autor...: DBNet - Toninho  Outubro/2004
*/

IF LENGTH(SELF:SCREEN-VALUE) = 5 AND
   SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) <> " " THEN DO.
   BELL.
   APPLY 'backspace' TO SELF.
   RETURN NO-APPLY.
END.

IF LENGTH(SELF:SCREEN-VALUE) >= 6 AND
   (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
    SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
   BELL.
   APPLY 'backspace' TO SELF.
   RETURN NO-APPLY.
END.
