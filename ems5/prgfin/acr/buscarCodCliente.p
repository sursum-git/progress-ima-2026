DEFINE INPUT  PARAMETER cNomeAbrev AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER iCliente   AS INTEGER     NO-UNDO.

FIND FIRST ems5.cliente
    WHERE cliente.nom_abrev = cNomeAbrev NO-LOCK NO-ERROR.
IF AVAIL cliente THEN
   ASSIGN iCliente = cliente.cdn_cliente.


