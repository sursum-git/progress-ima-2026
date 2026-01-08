DEF TEMP-TABLE tt-aux 
    FIELD it-codigo AS CHAR
    FIELD cod-refer AS CHAR
    FIELD c-desc-item AS CHAR
    FIELD de-qtd-ems  AS DEC
    FIELD de-qtd-etq  AS DEC
    FIELD de-dif      AS dec
    FIELD tipo-trans  AS INTEGER.

DEF VAR c-erro AS CHAR.

INPUT FROM c:\temp\essp0142-full.tmp.
REPEAT.
    CREATE tt-aux.
    IMPORT DELIMITER ";" tt-aux.
END.
INPUT CLOSE.


FOR EACH tt-aux WHERE
         ABS(tt-aux.de-dif) <= 5 NO-LOCK.

    FIND ITEM WHERE
         ITEM.it-codigo = tt-aux.it-codigo NO-LOCK NO-ERROR.

    IF ITEM.un = 'un' THEN NEXT.

    DISP tt-aux.it-codigo
         tt-aux.cod-refer
         tt-aux.de-dif
         ITEM.un.

    IF tt-aux.de-dif < 0 THEN
       ASSIGN tt-aux.tipo-trans = 1.
    ELSE
       ASSIGN tt-aux.tipo-trans = 2.


    ASSIGN c-erro = ''.
    RUN esapi/cria-movto-estoq.p (INPUT '1',
                                  INPUT tt-aux.it-codigo,
                                  INPUT tt-aux.cod-refer,
                                  INPUT tt-aux.cod-refer, 
                                  INPUT ABS(tt-aux.de-dif),
                                  INPUT 6,   /* DIV */
                                  INPUT tt-aux.tipo-trans,  
                                  INPUT "Acerto de Estoque x Etiquetas",
                                  OUTPUT c-erro). 

     IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
        MESSAGE "ERRO ao Efetuar a TD do Estoque:" SKIP 
                tt-aux.it-codigo 
                tt-aux.cod-refer
                tt-aux.de-dif SKIP
                c-erro 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     END.
END.
