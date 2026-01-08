/* Programa: upc-pd4000l2.p
** Objetivo: Trigger de 'Leave' para o campo fi-lote (Lote)
**           Verificar se o Lote est† no padr∆o
** Autor...: Prodb - Toninho  Maráo/2004
*/

IF LOOKUP(SUBSTR(SELF:SCREEN-VALUE,1,2),"PP,PD,RP,RD,CA") = 0 THEN DO.
   MESSAGE "Lote deve inicar com PP,PD,RP,RD,CA" VIEW-AS ALERT-BOX.
   APPLY 'entry' TO SELF.
   RETURN NO-APPLY. 
END.


