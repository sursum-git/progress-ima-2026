DEF BUFFER b-movto FOR movto-etq.

FOR EACH movto-etq WHERE
         movto-etq.cod-estab = '2' AND
         movto-etq.dt-trans = 04.25.2011 AND
         TRIM(entry(2,substr(movto-etq.char-1,INDEX(movto-etq.char-1,"Usuario:"),20),":")) = "super"
        BY movto-etq.num-etiqueta.

    /*IF INT(entry(2,substr(movto-etq.char-1,INDEX(movto-etq.char-1,"Ant:"),6),":")) <> 5 THEN NEXT.*/
                                         
    FIND ob-etiqueta WHERE 
         ob-etiqueta.cod-estab = movto-etq.cod-estab AND
         ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-ERROR.

    DISP int(ob-etiqueta.situacao)
         ob-etiqueta.cod-estab
         ob-etiqueta.dt-emiss
         ob-etiqueta.nr-lote
         movto-etq.num-etiqueta
         movto-etq.esp-docto (COUNT).

    ASSIGN ob-etiqueta.situacao = 5.
         


    /*
    FIND b-movto WHERE
         ROWID(b-movto) = ROWID(movto-etq) NO-LOCK NO-ERROR.

    FIND PREV b-movto WHERE
              b-movto.num-etiqueta = movto-etq.num-etiqueta AND
              b-movto.dt-trans < movto-etq.dt-trans
              NO-LOCK NO-ERROR.

    IF NOT AVAIL b-movto THEN NEXT.


    IF b-movto.esp-docto <> 'fat' THEN NEXT.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estab = movto-etq.cod-estab AND
         ob-etiqueta.num-etiqueta = movto-etq.num-etiqueta NO-ERROR.

    DISP movto-etq.dt-trans
         ob-etiqueta.cod-estab
         ob-etiqueta.num-etiqueta
         ob-etiqueta.quantidade (TOTAL)
         ob-etiqueta.it-codigo
         ob-etiqueta.cod-refer
         INT(ob-etiqueta.situacao)
         /*INT(entry(2,substr(movto-etq.char-1,INDEX(movto-etq.char-1,"ova:"),6),":")) */
         WITH WIDTH 300.
         */
         
END.
