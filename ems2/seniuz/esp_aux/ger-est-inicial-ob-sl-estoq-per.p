DEF VAR c-periodo AS CHAR.
DEF VAR c-per-ant AS CHAR.
DEF VAR i-mes AS INT.
DEF VAR i-ano AS INT.


ASSIGN c-periodo = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999').

ASSIGN i-mes = INT(SUBSTR(c-periodo,1,2)) - 1
       i-ano = INT(SUBSTR(c-periodo,3,4)).
IF i-mes = 0  THEN
   ASSIGN i-mes = 12
          i-ano = i-ano - 1.
ASSIGN c-per-ant = STRING(i-mes, '99') + STRING(i-ano, '9999').

/* Limpar o periodo */
FOR EACH ob-sl-estoq-per WHERE
         ob-sl-estoq-per.periodo = c-periodo SHARE-LOCK.
    DELETE ob-sl-estoq-per.
END.

/* Gerando Estoque Atual */
FOR EACH  ob-etiqueta WHERE 
          ob-etiqueta.situacao >= 3 AND  
          ob-etiqueta.situacao <= 4 NO-LOCK.

    IF ob-etiqueta.dt-emissao = 03/07/2008 AND
       ob-etiqueta.situacao   = 3 THEN NEXT.

    FIND ob-sl-estoq-per WHERE
         ob-sl-estoq-per.periodo      = c-periodo               AND
         ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
         ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
         ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
         ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
         ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid  NO-ERROR.
    IF NOT AVAIL ob-sl-estoq-per THEN DO:
       CREATE ob-sl-estoq-per.
       ASSIGN ob-sl-estoq-per.periodo      = c-periodo
              ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
              ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
              ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
              ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
              ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid.
    END.
    ASSIGN ob-sl-estoq-per.qtd-inicial = ob-sl-estoq-per.qtd-inicial +
                                         ob-etiqueta.quantidade.

END.

