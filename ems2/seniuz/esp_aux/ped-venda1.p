/* Programa: ped-venda1.p
** Troca transportador de pedidos
*/

DEF VAR c-orig LIKE transporte.nome-abrev.
DEF VAR c-dest LIKE transporte.nome-abrev.
DEF VAR i-cont AS INT.
DEF VAR l-acao AS LOG FORMAT "Alterar/Simular".
DEF VAR l-export AS LOG FORMAT "Sim/NÆo".

FIND transporte WHERE transporte.cod-transp = 253 NO-LOCK.
ASSIGN c-orig = transporte.nome-abrev.
FIND transporte WHERE transporte.cod-transp = 414 NO-LOCK.
ASSIGN c-dest = transporte.nome-abrev.

DISP c-orig
     c-dest.
UPDATE l-acao
       l-export.

FOR EACH ped-venda WHERE (ped-venda.cod-sit-ped < 3 OR
                          ped-venda.cod-sit-ped = 5)
                     AND ped-venda.nome-transp = c-orig,
    EACH emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                    AND  ((emitente.natureza < 3 AND l-export = NO) OR
                                                     l-export = YES)
                  NO-LOCK.
    
    IF l-acao = YES THEN
       ASSIGN ped-venda.nome-transp = c-dest.
    ELSE
       DISP ped-venda.nr-pedcli
            ped-venda.nome-transp.

    ASSIGN i-cont = i-cont + 1.
END.
DISP c-orig
     c-dest
     i-cont.
