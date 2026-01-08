/********************************************************** ************************************************* 
    Programa....: lbapi-trata-pro.p
    
    Objetivo....: Realizar o tratamento da string de peso
    
    Empresa.....: LABTEST
    
    Comment.....: Realiza o tratamento da string apos captura de balança 9091
                      
    Parametros..: nenhum
    
    Criacao....: 28/07/2008 - ROMULO DA SILVA PEREIRA
                                      
********************************************************* **************************************************/ 


DEFINE VAR c-peso           AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE VAR i-tentativas     AS INT INIT 1   NO-UNDO.
DEFINE VAR l-repetirCaptura AS LOG INIT YES NO-UNDO.

REPEAT WHILE l-repetirCaptura :
    RUN esp_aux\captura-peso.p(OUTPUT c-peso).
    RUN pi-trata-peso(INPUT-OUTPUT c-peso).
    i-tentativas = i-tentativas + 1.
END.

MESSAGE c-peso
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


PROCEDURE pi-trata-peso:
    DEFINE INPUT-OUTPUT PARAM temp-peso  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE temp-peso2 AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c-numeros AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE tam AS INTEGER NO-UNDO.
    DEFINE VARIABLE ini AS INTEGER NO-UNDO.
    DEFINE VARIABLE i   AS INTEGER INIT 0 NO-UNDO.
    
    assign 
        c-numeros = "0123456789"
        l-repetirCaptura = NO.
    
    IF temp-peso <> "" THEN DO:
        tam = LENGTH(temp-peso).
        i   = 1.
        
        REPEAT WHILE SUBSTR(temp-peso,tam,1) <> SUBSTR(c-numeros,i,1):
        IF i = 10 THEN DO:
            i = 1.
            tam = tam - 1.
        END.
        ELSE
            i = i + 1.
        END.
        
        assign
            ini = index(temp-peso,"`") + 1
            tam = tam - ini.
        
        IF tam < 8 AND i-tentativas < 10 then
            l-repetirCaptura = YES.
        
        temp-peso2 = SUBSTR(temp-peso,ini,tam).
        
        temp-peso = SUBSTR(temp-peso2,1,4) + "," + SUBSTR(temp-peso2,5,12) .

    END.
END.
