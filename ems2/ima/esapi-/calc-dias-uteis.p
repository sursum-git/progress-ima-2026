/************************************************************************
 API  - calc-dias-uteis.p
 Autor - Daniel Ferraz = 08/03/2013
 Objetivo - Calcular dias uteis entre duas datas.
 
 Exemplo de Execu‡Æo
 RUN esapi/calc-dias-uteis.p (INPUT ,           /* data inicial */
                              INPUT ,           /*  data final  */
                              OUTPUT p-atraso).   /* data calculada */ 
                              
************************************************************************/

DEF INPUT PARAMETER p-dt-inicio AS DATE FORMAT "99/99/9999".
DEF INPUT PARAMETER p-dt-fim AS DATE FORMAT "99/99/9999".
DEF OUTPUT PARAMETER p-atraso AS INT.

FIND FIRST para-ped NO-LOCK NO-ERROR.

FOR EACH calen-coml WHERE 
         calen-coml.cod-estabel = para-ped.estab-padrao AND
         calen-coml.data >= p-dt-inicio AND
         calen-coml.data <= p-dt-fim AND
         calen-coml.tipo = 1 NO-LOCK.

    ASSIGN p-atraso = p-atraso + 1.
END.


