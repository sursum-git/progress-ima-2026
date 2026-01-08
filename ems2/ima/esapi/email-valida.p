/*Codigo para testar email
DEFINE new SHARED VARIABLE email-validado AS LOGICAL   NO-UNDO.
DEFINE new SHARED VARIABLE email          AS CHARACTER NO-UNDO.
assign email = "imatextil@imatextil.com.br".
run esrp/valida-email.p.
MESSAGE email-validado
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

/* Variaveis para transferencias de dados para validar email*/
DEFINE INPUT  PARAMETER email          AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER email-validado AS LOGICAL   NO-UNDO.

/* Variaveis auxiliares para valida‡Æo do email */
DEFINE VARIABLE i                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE qt-arroba              AS INTEGER   NO-UNDO.

ASSIGN email-validado = YES
       email = TRIM(email).

IF email = ""                        OR  
   NOT email MATCHES("*@*")          OR
   NOT email MATCHES("*.*")          OR
       email MATCHES("*@.com*")      OR 
       email MATCHES("*@com.br")     OR
       email MATCHES("*@hotmail.br") OR
       email MATCHES("*@homail*")    OR 
       email MATCHES("*@hottmail*")  OR
       email MATCHES("*@yaho.com*")  OR 
       email MATCHES("*@otmail*")    OR 
       email MATCHES("*@igi")        OR
       email MATCHES("*mial*")       OR
       email MATCHES("www.*")        THEN DO:
   ASSIGN email-validado = NO.
END.
ELSE DO:
   IF email MATCHES("*.com*") OR 
      email MATCHES("*.net*") OR 
      email MATCHES("*.org*") OR
      email MATCHES("*.br")   THEN DO:

      ASSIGN qt-arroba = 0.
      DO i = 1 to LENGTH(email):
         IF ASC(SUBSTRING(email,i,1)) >= 44 AND ASC(SUBSTRING(email,i,1)) <= 57  OR 
            ASC(SUBSTRING(email,i,1)) >= 64 AND ASC(SUBSTRING(email,i,1)) <= 90  OR
            ASC(SUBSTRING(email,i,1)) >= 97 AND ASC(SUBSTRING(email,i,1)) <= 122 OR 
            ASC(SUBSTRING(email,i,1)) >= 64 AND ASC(SUBSTRING(email,i,1)) <= 90  OR 
            SUBSTRING(email,i,1) = "_" THEN DO:
            IF SUBSTRING(email,i,1) = "@" THEN DO:
                ASSIGN qt-arroba = qt-arroba + 1.
                IF qt-arroba > 1 THEN DO:
                   ASSIGN email-validado = NO.
                  LEAVE.
                END.
            END.
         END.
         ELSE DO:
              ASSIGN email-validado = NO.
              LEAVE.  
         END.
      END.
   END.
   ELSE DO:
        ASSIGN email-validado = NO.
        LEAVE.
   END.
END.
