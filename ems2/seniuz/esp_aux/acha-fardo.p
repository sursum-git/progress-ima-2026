DEF VAR c-placa    AS CHAR FORMAT "!!!-9999".
DEF VAR da-dt-entr AS DATE FORMAT "99/99/9999".
DEF VAR prim-fardo AS INT  FORMAT "9999,9999".
DEF VAR ult-fardo  AS INT  FORMAT "9999,9999".

REPEAT ON ENDKEY UNDO,LEAVE:
   UPDATE c-placa LABEL "Placa"
          da-dt-entr LABEL "Data Entrada".
   FIND mp-entr-cam WHERE mp-entr-cam.placa      = c-placa AND
                          mp-entr-cam.dt-entrada = da-dt-entr NO-LOCK NO-ERROR.
   IF NOT AVAIL mp-entr-cam THEN DO:
      MESSAGE "Entrada Veiculo n∆o encontrada!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      UNDO,RETRY.
   END.

   FIND mp-entr-mat WHERE mp-entr-mat.nr-cdr = mp-entr-cam.nr-cdr. NO-LOCK NO-ERROR.
   IF NOT AVAIL mp-entr-mat THEN DO:
      MESSAGE "Entrada dos Fardos n∆o encontrada!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      UNDO,RETRY.
   END.

   ASSIGN prim-fardo = 0.
   FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr NO-LOCK.
        IF prim-fardo = 0 THEN
            ASSIGN prim-fardo = mp-fardo.nr-fardo.
        ASSIGN ult-fardo = mp-fardo.nr-fardo.
   END.

   DISP prim-fardo LABEL "1ß Fardo"
        ult-fardo  LABEL "Ultimo Fardo".

   ASSIGN c-placa = ""
          da-dt-entr = date("  /  /    ").
END.

