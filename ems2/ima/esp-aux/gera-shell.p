DEF VAR c-arq-shell AS CHAR.
/* PRD */
DEF VAR c-arq-dump  AS CHAR INIT "/datasul/totvs/database/dump".
DEF VAR c-arq-load  AS CHAR INIT "/datasul/totvs/database/dump".
DEF VAR c-temp-dump AS CHAR INIT "d:\dump\gera-dump-".
DEF VAR c-temp-load AS CHAR INIT "d:\dump\gera-load-".

/* TST
DEF VAR c-arq-dump  AS CHAR INIT "/wrk/totvs/datasul/totvs11/database/dump".
DEF VAR c-arq-load  AS CHAR INIT "/wrk/totvs/datasul/totvs11/database/dump".
DEF VAR c-temp-dump AS CHAR INIT "c:\temp\dumpTST\gera-dump-".
DEF VAR c-temp-load AS CHAR INIT "c:\temp\dumpTST\gera-load-".
*/

/*
====> Para os bancos emsdev, finance, mdmerge, mdtfrw e payroll s’o SQL -> o dump ² feito por outro processo.
*/
ASSIGN c-arq-shell = c-temp-dump + PDBNAME("dictdb") + ".sh".

OUTPUT TO VALUE(c-arq-shell).
PUT UNFORMATTED
    "mkdir " + c-arq-dump + "/" + PDBNAME("dictdb") SKIP. 

FOR EACH _File WHERE 
         _File._File-Number > 0 AND 
         _File._File-Number < 32768 NO-LOCK:
    PUT UNFORMATTED 
        "sh proutil " + PDBNAME("dictdb") + " -C dump " + _File._File-Name + " " + c-arq-dump + "/" + PDBNAME("dictdb")
        SKIP.
END.
OUTPUT CLOSE.

ASSIGN c-arq-shell = c-temp-load + PDBNAME("dictdb") + ".sh".
OUTPUT TO VALUE(c-arq-shell).

FOR EACH _File WHERE 
         _File._File-Number > 0 AND 
         _File._File-Number < 32768 NO-LOCK.
    PUT UNFORMATTED 
        "sh proutil " + PDBNAME("dictdb") + " -C load " + c-arq-load + "/" + PDBNAME("dictdb") + "/" _File._File-Name + ".bd" 
        SKIP.
END.
OUTPUT CLOSE.
