OUTPUT TO c:/temp/upc.csv.
FOR EACH prog_dtsul WHERE prog_dtsul.nom_prog_upc <> "" NO-LOCK:
    PUT prog_dtsul.cod_prog_dtsul ";"
        prog_dtsul.nom_prog_upc SKIP.
END.
OUTPUT CLOSE.
