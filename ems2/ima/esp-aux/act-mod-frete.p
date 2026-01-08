FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '502' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-nota-fis = '0000044' SHARE-LOCK NO-ERROR.

ASSIGN SUBSTRING(nota-fiscal.char-2,203,1) = '0'.


