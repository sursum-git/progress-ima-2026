DISABLE TRIGGERS FOR DUMP OF dbaux.loc-entr.
DISABLE TRIGGERS FOR LOAD OF dbaux.loc-entr.

/* Parameters Definitions */
DEFINE PARAMETER BUFFER b-loc-entr FOR mgind.loc-entr.

/* Defini‡Æo de Variaveis */
FIND dbaux.loc-entr WHERE
     dbaux.loc-entr.nome-abrev = b-loc-entr.nome-abrev AND
     dbaux.loc-entr.cod-entrega = b-loc-entr.cod-entrega NO-ERROR.
IF NOT AVAIL dbaux.loc-entr THEN
   CREATE dbaux.loc-entr.

BUFFER-COPY b-loc-entr TO dbaux.loc-entr.

