/* Programa: upcw-di053.p
** Objetivo: Alterar a Data de Vencimento das Duplicatas de acordo com os dias 
**           de vencimento fixos, informados na condi‡Æo de pagamento especial do 
**           pedido de venda (cond-ped, ped-venda).
** Autor...: Antonio G. Souza (Toninho) - SeniuZ
** Data....: 25/Fev/2008
*/

DEFINE PARAMETER BUFFER p-table FOR fat-duplic.
DEFINE PARAMETER BUFFER p-table-old FOR fat-duplic.

DEF VAR c-periodo AS CHAR.
DEF VAR c-ult-dia-mes AS CHAR.
DEF VAR da-dt-calc AS DATE.
DEF VAR i-dia AS INT.

FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = p-table.cod-estabel  AND
     nota-fiscal.serie =  p-table.serie AND   
     nota-fiscal.nr-nota-fis = p-table.nr-fatura  
     NO-LOCK NO-ERROR.

IF AVAIL nota-fiscal THEN DO.
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = nota-fiscal.nr-pedcli AND
        ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
        NO-LOCK NO-ERROR.

    FIND FIRST cond-ped OF ped-venda NO-ERROR.
    IF AVAIL cond-ped AND cond-ped.char-2 <> '' THEN DO.
       ASSIGN da-dt-calc = ?.
       DO i-dia = 1 TO NUM-ENTRIES(cond-ped.char-2,";").
          IF INT(ENTRY(i-dia,cond-ped.char-2,";")) > DAY(p-table.dt-vencim) THEN
             ASSIGN da-dt-calc = p-table.dt-vencim - DAY(p-table.dt-vencim) + INT(ENTRY(i-dia,cond-ped.char-2,";")).

          IF da-dt-calc <> ? THEN LEAVE.
       END.
    
       IF da-dt-calc = ? THEN DO.
          ASSIGN c-periodo = STRING(MONTH(p-table.dt-vencim),"99") + STRING(YEAR(p-table.dt-vencim),"9999").
          RUN esapi/ret-udm.p (INPUT c-periodo, OUTPUT c-ult-dia-mes).
    
          ASSIGN da-dt-calc = p-table.dt-vencim - DAY(p-table.dt-vencim) + INT(c-ult-dia-mes) + INT(ENTRY(1,cond-ped.char-2,";")).
       END.

       IF da-dt-calc <> ? THEN
          ASSIGN p-table.dt-vencim = da-dt-calc.
    END.
END.

