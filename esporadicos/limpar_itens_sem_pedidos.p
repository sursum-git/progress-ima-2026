FOR EACH ped-item
WHERE YEAR(dt-entrega) = 2020
AND ped-item.cod-sit-item = 1:
    FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
       DISP ped-item.nr-pedcli.
       FIND ped-ent OF ped-item NO-ERROR.
       IF AVAIL ped-ent THEN
          DELETE ped-ent.
       DELETE ped-item.
    END.
END.
