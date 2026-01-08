DEF VAR c-cod-estabel   LIKE nota-fiscal.cod-estabel.
DEF VAR c-serie         LIKE nota-fiscal.serie INITIAL "".
DEF VAR c-nr-nota-fis   LIKE nota-fiscal.nr-nota-fis.

UPDATE c-cod-estabel c-serie c-nr-nota-fis WITH 1 COL.

FOR FIRST nota-fiscal EXCLUSIVE-LOCK
    WHERE nota-fiscal.cod-estabel = c-cod-estabel
    AND nota-fiscal.serie         = c-serie
    AND nota-fiscal.nr-nota-fis   = c-nr-nota-fis
    AND nota-fiscal.dt-cancel    <> ?:
    
    FIND FIRST sit-nf-eletro USE-INDEX stnfltr-id
                   where sit-nf-eletro.cod-nota-fisc = c-nr-nota-fis 
                     AND sit-nf-eletro.cod-estabel = c-cod-estabel 
                     AND sit-nf-eletro.cod-serie = c-serie EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAIL sit-nf-eletro THEN
                   ASSIGN sit-nf-eletro.idi-sit-nf-eletro = 7.
    ELSE DO:
        CREATE sit-nf-eletro.
        ASSIGN sit-nf-eletro.cod-nota-fisc     = c-nr-nota-fis
               sit-nf-eletro.cod-estabel       = c-cod-estabel  
               sit-nf-eletro.cod-serie         = c-serie 
               sit-nf-eletro.idi-sit-nf-eletro = 7.
         END.
END.
