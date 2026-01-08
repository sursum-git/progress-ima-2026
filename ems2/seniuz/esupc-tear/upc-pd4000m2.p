/* Programa: upc-pd4000m2.p
** Objetivo: Trigger de 'Mouse-Select-Click' para o botÆo que Salva a Condi‡Æo
**           de pagamento Especial, afim de verificar o limite m ximo de 10
**           Duplicatas por pedido
** Autor...: DbNET - Toninho  Mar‡o/2006
*/

DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.
DEF VAR i-ct AS INT.

ASSIGN i-ct = 0.
FOR EACH cond-ped WHERE
         cond-ped.nr-pedido = INT(h-nr-pedcli:SCREEN-VALUE)  NO-LOCK.
    ASSIGN i-ct = i-ct + 1.     
END.

IF i-ct >= 9 THEN DO.
   MESSAGE "Permitido no m ximo 9 duplicatas por Pedido..."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN NO-APPLY.
END.

APPLY 'choose' TO SELF.
