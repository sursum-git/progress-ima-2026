DEFINE PARAMETER BUFFER p-table FOR repres.

FOR EACH cota-rep OF repres.
    DELETE cota-rep.
END.

FIND repres-ext OF p-table NO-ERROR.
IF AVAIL repres-ext THEN
   DELETE repres-ext.


