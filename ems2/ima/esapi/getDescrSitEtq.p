//retorna a descri‡Æo da situa‡Æo da etiqueta

DEFINE INPUT  PARAMETER pSituacao AS INTEGER     NO-UNDO.
DEFINE OUTPUT  PARAMETER cSituacao AS CHARACTER   NO-UNDO.

CASE pSituacao:

    WHEN 1 THEN
        assign cSituacao = 'Impressa'.
    WHEN 2 THEN
        assign cSituacao = 'Em Produ‡Æo'.
    WHEN 3 THEN
        assign cSituacao = 'Em Estoque'.
    WHEN 4 THEN
        assign cSituacao = 'Reservada'.
    WHEN 5 THEN
        assign cSituacao = 'Faturada'.
    WHEN 6 THEN
        assign cSituacao = 'Em Reprocesso'.
    WHEN 7 THEN
        assign cSituacao = 'Consum.Corte'.
    WHEN 8 THEN
        assign cSituacao = 'Bloqueado'.
    WHEN 9 THEN
        assign cSituacao = 'Consumido'.




END CASE.
