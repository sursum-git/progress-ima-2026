TRIGGER PROCEDURE FOR WRITE OF ped-item-ext OLD BUFFER ped-item-ext-old.

IF SEARCH ("estrg/trw/trwes013p.p") <> ? THEN
   RUN estrg/trw/trwes013p.p (BUFFER ped-item-ext, BUFFER ped-item-ext-old).

