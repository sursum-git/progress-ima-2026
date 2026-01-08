TRIGGER PROCEDURE FOR DELETE OF pp-ped-venda.

FOR EACH pp-ped-item WHERE
         pp-ped-item.nr-pedcli = pp-ped-venda.nr-pedcli EXCLUSIVE-LOCK.
    DELETE pp-ped-item.
END.


