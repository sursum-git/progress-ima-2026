DEF VAR c-situacao AS CHAR FORMAT "x(15)".
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  "2"
                       AND nota-fiscal.serie        =  "1"
                       AND nota-fiscal.dt-emis-nota >= 09/01/2005
                       /*AND nota-fiscal.dt-cancela   =  ?*/
                       AND nota-fiscal.ind-sit-nota <  7
                     NO-LOCK.

   {esinc/i-dsrb.i nota-fiscal.ind-sit-nota nota-fiscal.ind-sit-nota c-situacao} 
   ASSIGN c-situacao = string(nota-fiscal.ind-sit-not,'99') + '-' + c-situacao.

    DISP nota-fiscal.nr-nota-fis
         nota-fiscal.dt-emis-nota
         c-situacao.
END.
