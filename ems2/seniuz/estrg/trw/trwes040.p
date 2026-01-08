TRIGGER PROCEDURE FOR WRITE OF ped-item-res OLD BUFFER ped-item-res-old.

IF SEARCH ("estrg/trw/trwes040p.p") <> ? THEN
   RUN estrg/trw/trwes040p.p (BUFFER ped-item-res, BUFFER ped-item-res-old).

