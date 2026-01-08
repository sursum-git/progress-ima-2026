/* Include i-epc200.i: Defini‡Æo Temp-Table tt-epc */
{include/i-epc200.i1}

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

MESSAGE 'oi' VIEW-AS ALERT-BOX.

/*
for each tt-epc no-lock
    where tt-epc.cod-event = p-ind-event:
    /* Exemplo de uso dos parƒmetros */
    disp tt-epc. 
end.
*/
