DEFINE PARAMETER BUFFER p-table FOR transporte.
DEFINE PARAMETER BUFFER p-table-old FOR transporte.

IF p-table.nome-abrev <> p-table-old.nome-abrev THEN DO.
   FOR EACH transporte-ext WHERE
            transporte-ext.nome-transp = p-table-old.nome-abrev EXCLUSIVE-LOCK.
       ASSIGN transporte-ext.nome-transp = p-table.nome-abrev.
   END.
END.

