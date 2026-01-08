/************************************************************************
 API  - soma-dias-uteis.p
 Autor - Toninho = 15/02/2013
 Objetivo - Somar dias uteis em uma determinada data
 
 Exemplo de Execu‡Æo
 RUN esapi/soma-dias-uteis.p (INPUT 3,           /* qtd dias a somar */
                              INPUT TODAY,       /* data de inicio */
                              OUTPUT dt-calc).   /* data calculada */ 
                              
************************************************************************/

DEF INPUT PARAMETER p-dias AS INT.
DEF INPUT PARAMETER p-dt-inicio AS DATE FORMAT "99/99/9999".
DEF OUTPUT PARAMETER p-data AS DATE FORMAT "99/99/9999".

DEF VAR i-ct AS INT.

FIND FIRST para-ped NO-LOCK NO-ERROR.

FOR EACH calen-coml WHERE 
         calen-coml.cod-estabel = para-ped.estab-padrao AND
         calen-coml.data >= p-dt-inicio AND
         calen-coml.tipo = 1 NO-LOCK 
         BY calen-coml.data.

    ASSIGN i-ct = i-ct + 1.

    IF i-ct > p-dias THEN DO.
       ASSIGN p-data = calen-coml.data.
       LEAVE.
    END.
END.


