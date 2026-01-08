DEF VAR i-cont AS INT.
DEF VAR i-seq  AS INT INIT 1.
DEF VAR c-arquivo AS CHAR.

ASSIGN c-arquivo = "/mnt/dump_ems/mov/in220_" + STRING(i-seq,"99") + ".d".

OUTPUT TO VALUE(c-arquivo).

FOR EACH movto-mat NO-LOCK:
    EXPORT movto-mat.
    ASSIGN i-cont = i-cont + 1.
    IF i-cont = 4000000 THEN do:
       PUT UNFORMAT
           "." SKIP
           "PSC" SKIP
           "filename=movto-mat" SKIP
           "records=" + STRING(i-cont,"9999999999999") SKIP
           "ldbname=mgmov" SKIP
           "timestamp=" + STRING(YEAR(TODAY),"9999") + "/" +
                          STRING(MONTH(TODAY),"99") + "/" +
                          STRING(DAY(TODAY),"99") + "-" +
                          STRING(TIME,"hh:mm:ss") SKIP
           "numformat=46,44" SKIP
           "dateformat=dmy-1950" SKIP
           "map=NO-MAP" SKIP
           "cpstream=ibm850" SKIP
           "." SKIP
           "0000004046" SKIP.
       OUTPUT CLOSE.
       ASSIGN i-seq  = i-seq + 1
              i-cont = 0.
       ASSIGN c-arquivo = "/mnt/dump_ems/mov/in220_" + STRING(i-seq,"99") + ".d".
       OUTPUT TO VALUE(c-arquivo).
    END.
END.

IF i-cont > 0 THEN
   PUT UNFORMAT
       "." SKIP
       "PSC" SKIP
       "filename=movto-mat" SKIP
       "records=" + STRING(i-cont,"9999999999999") SKIP
       "ldbname=mgmov" SKIP
       "timestamp=" + STRING(YEAR(TODAY),"9999") + "/" +
                      STRING(MONTH(TODAY),"99") + "/" +
                      STRING(DAY(TODAY),"99") + "-" +
                      STRING(TIME,"hh:mm:ss") SKIP
       "numformat=46,44" SKIP
       "dateformat=dmy-1950" SKIP
       "map=NO-MAP" SKIP
       "cpstream=ibm850" SKIP
       "." SKIP
       "0000004046" SKIP.
OUTPUT CLOSE.

QUIT.

