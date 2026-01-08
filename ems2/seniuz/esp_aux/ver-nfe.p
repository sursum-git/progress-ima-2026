FIND FIRST sit-nf-eletro NO-LOCK
     WHERE sit-nf-eletro.cod-estabel   = '2' 
       AND sit-nf-eletro.cod-serie     = '1'
       AND sit-nf-eletro.cod-nota-fisc = '0170065' NO-ERROR.

ASSIGN sit-nf-eletro.idi-sit-nf-eletro
