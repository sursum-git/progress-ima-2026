FIND FIRST sit-nf-eletro EXCLUSIVE-LOCK
     WHERE sit-nf-eletro.cod-estabel   = '2'
       AND sit-nf-eletro.cod-serie     = '3'
       AND sit-nf-eletro.cod-nota-fisc = '0000003'
     NO-ERROR.
DISP sit-nf-eletro.idi-sit-nf-eletro VIEW-AS FILL-IN.
/*ASSIGN sit-nf-eletro.idi-sit-nf-eletro = 6.*/
