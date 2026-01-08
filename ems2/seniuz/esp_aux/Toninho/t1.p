/* MED ultima nota 0066745 */

FOR EACH nota-fiscal WHERE
         nota-fiscal.cod-estabel = '5' AND
         nota-fiscal.serie = '3' AND
         nota-fiscal.dt-emis >= 01.06.2016 NO-LOCK.

    IF nota-fiscal.dt-cancela <> ? THEN NEXT.

    FOR EACH it-nota-fisc OF nota-fiscal SHARE-LOCK.
        IF it-nota-fisc.val-desconto-total > 0 THEN DO.
           DISP nota-fiscal.nr-nota-fis
                nota-fiscal.dt-emis
                it-nota-fisc.val-desconto-total.
        END.
    END.
END.

