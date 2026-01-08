TRIGGER PROCEDURE FOR DELETE OF ped-item-res.

IF SEARCH ("estrg/trd/trdes040p.r") <> ? THEN
   RUN estrg/trd/trdes040p.r (BUFFER ped-item-res).

