FOR EACH tit-ap WHERE tit-ap.cod-fornec = 4135
                  AND tit-ap.cod-esp    = "nf"
                  AND tit-ap.dt-trans   >= 01/01/2010 NO-LOCK.
    DISP tit-ap.nr-docto
         tit-ap.cod-esp
         tit-ap.dt-trans
         tit-ap.vl-original(TOTAL).
END.
