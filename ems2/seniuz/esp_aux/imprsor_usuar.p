DEF VAR c-lixo LIKE imprsor_usuar.nom_disposit_so.
FOR EACH imprsor_usuar WHERE imprsor_usuar.nom_impressora BEGINS "Lex" /*T632N1".*/
                         AND (imprsor_usuar.nom_disposit_so MATCHES "*hapi*" OR
                             imprsor_usuar.nom_disposit_so MATCHES "*maat*").
    /* 
    imprsor_usuar WHERE imprsor_usuar.nom_disposit_so MATCHES "*maat*". /*T632N1".*/
    */
    ASSIGN c-lixo = REPLACE(imprsor_usuar.nom_disposit_so,"hapi","anuket").
    DISP imprsor_usuar.nom_impressora
         imprsor_usuar.cod_usuario
         imprsor_usuar.nom_disposit_so
         c-lixo
         "---"
         WITH SIDE-LABELS 1 COLUMN WIDTH 300.
    /*ASSIGN imprsor_usuar.nom_disposit_so = c-lixo.*/
END.
