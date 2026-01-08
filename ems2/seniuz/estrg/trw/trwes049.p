TRIGGER PROCEDURE FOR WRITE OF ob-etiqueta OLD BUFFER ob-etiqueta-old.

IF SEARCH ("estrg/trw/trwes049p.p") <> ? THEN
   RUN estrg/trw/trwes049p.p (BUFFER ob-etiqueta, BUFFER ob-etiqueta-old).

