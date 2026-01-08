DEFINE TEMP-TABLE tt
    FIELD campo AS CHAR FORMAT 'x(300)' EXTENT 5.
DEFINE VARIABLE iMsg AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE lErro AS LOGICAL     NO-UNDO.
INPUT FROM c:\temp\ext-emitente.txt.
REPEAT :
    CREATE tt.
    IMPORT DELIMITER " " tt.
END.

REPEAT iCont = 1 TO 3:
    IF iCont = 1 THEN DO:
        IF CONNECTED('dbaux') THEN
            DISCONNECT dbaux.
    END.
    IF iCont = 2 THEN DO:
       RUN esporadicos/conecta_bases.p .
       IF CONNECTED('ems5bkp') THEN
          DISCONNECT ems5bkp.
    END.
       
    IF iCont = 3 THEN
       RUN esporadicos/conecta_base_ems5bkp.p .

    OUTPUT TO value('c:\temp\ext-emitente-compila_' + STRING(icont) + '.txt') .
    FOR EACH tt.
        //DISP tt.campo[1] WITH WIDTH 550. PAUSE 0.
        ASSIGN lErro = NO.
        COMPILE VALUE(tt.campo[1]) SAVE.
        DO iMsg = 1 TO ERROR-STATUS:NUM-MESSAGES:
            IF ERROR-STATUS:ERROR THEN DO:
               EXPORT DELIMITER "|"  tt.campo[1] ERROR-STATUS:GET-NUMBER(iMsg).
               ASSIGN lErro = YES.    
               NEXT.
            END. 
           
            
        END.
        IF lErro THEN
           DELETE tt.
    END.  
    OUTPUT CLOSE.
END.
