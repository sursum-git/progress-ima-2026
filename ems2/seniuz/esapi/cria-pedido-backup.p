
DEFINE SHARED TEMP-TABLE tt-ped-venda LIKE htx-ped-venda.
DEFINE SHARED TEMP-TABLE tt-ped-item  LIKE htx-ped-item.

FOR EACH tt-ped-venda:
    CREATE ped-venda.
    BUFFER-COPY tt-ped-venda TO ped-venda.
END.

FOR EACH tt-ped-item:
    CREATE ped-item.
    BUFFER-COPY tt-ped-item TO ped-item.
END.
