FOR FIRST nota-fiscal EXCLUSIVE-LOCK
    WHERE nota-fiscal.cod-estabel = '5'
      AND nota-fiscal.serie       = '4'
      AND nota-fiscal.nr-nota-fis = '0000756':
    FOR FIRST sit-nf-eletro EXCLUSIVE-LOCK
        WHERE sit-nf-eletro.cod-estabel  = nota-fiscal.cod-estabel
          AND sit-nf-eletro.cod-serie    = nota-fiscal.serie
          AND sit-nf-eletro.cod-nota-fis = nota-fiscal.nr-nota-fis:
        DISP sit-nf-eletro.idi-sit-nf-eletro VIEW-AS FILL-IN.
/*         ASSIGN sit-nf-eletro.idi-sit-nf-eletro = 3.  */
    END.

/*     ASSIGN OVERLAY(nota-fiscal.char-1,97,15) = '131110303151160' /* Protocolo */                            */
/*            SUBSTR(nota-fiscal.char-2,3,60)   = '31110203123987000472550040000005070000000005'. /* Chave */  */

    UPDATE nota-fiscal.char-2 FORMAT "x(80)" WITH WIDTH 200. /* 18 brancos ap¢s a chave */
    UPDATE nota-fiscal.ind-sit-nota.
END.
