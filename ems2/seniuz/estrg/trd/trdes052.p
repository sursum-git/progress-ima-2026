TRIGGER PROCEDURE FOR DELETE OF ped-item-rom.

IF SEARCH ("estrg/trd/trdes052p.r") <> ? THEN
   RUN estrg/trd/trdes052p.r (BUFFER ped-item-rom).

