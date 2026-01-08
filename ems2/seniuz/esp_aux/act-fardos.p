FOR EACH mp-fardo WHERE
         mp-fardo.nr-fardo >= 09094848 AND
         mp-fardo.nr-fardo <= 09094982 SHARE-LOCK.
    ASSIGN mp-fardo.letra        = 'B'
           mp-fardo.cd-tipo      = 3
           mp-fardo.cd-coloracao = 1 
           mp-fardo.cd-compr     = 2
           mp-fardo.situacao     = 3.
END.
