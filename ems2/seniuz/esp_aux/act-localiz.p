 FOR EACH ob-etiqueta WHERE
          ob-etiqueta.situacao = 3 AND
          ob-etiqueta.localiz = '' AND
          ob-etiqueta.cod-estab = '1' SHARE-LOCK.

     IF SUBSTR(ob-etiqueta.char-1,50,200) MATCHES "*29/05*" THEN DO.
         /*DISP substr(entry(2,SUBSTR(ob-etiqueta.char-1,50,200),":"),1,6).*/
         ASSIGN ob-etiqueta.localiz = SUBSTR(ENTRY(2,SUBSTR(ob-etiqueta.char-1,50,200),":"),1,6).
     END.
     
 END.
