DEFINE INPUT  PARAMETER c-nome-abrev AS CHARACTER FORMAT "x(12)".
DEFINE INPUT  PARAMETER c-nr-pedcli  AS CHARACTER FORMAT "x(12)".
DEFINE INPUT  PARAMETER l-papel-pao  AS LOGICAL.
DEFINE OUTPUT PARAMETER l-gerou-ok   AS LOGICAL. 

DEFINE VARIABLE l-base-backup AS LOGICAL INITIAL NO NO-UNDO.

RUN esapi/connect-agatex.p.

RUN espaga/checa-backup.p (OUTPUT l-base-backup).

IF l-base-backup THEN DO:
    RUN espaga/esft4002a.p (INPUT  c-nome-abrev,
                            INPUT  c-nr-pedcli,
                            INPUT  l-papel-pao,
                            OUTPUT l-gerou-ok).
END.
ELSE
    ASSIGN l-gerou-ok = NO.

RUN esapi/connect-agatex.p.



