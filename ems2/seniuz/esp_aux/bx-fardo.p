DEF VAR i-num-etiqueta AS INT FORMAT ">>>>>>>>9" LABEL "N§ Etiqueta".
DEF VAR de-peso AS DEC.
REPEAT.
   CLEAR ALL.
   UPDATE i-num-etiqueta.
   FIND mp-fardo WHERE
        mp-fardo.nr-fardo = i-num-etiqueta SHARE-LOCK NO-ERROR.
   IF NOT AVAIL mp-fardo THEN DO:
      MESSAGE "NÆo Achei o FARDO ! ! "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      UNDO,RETRY.
   END.
   ASSIGN de-peso = de-peso + mp-fardo.peso.
   MESSAGE "N§ Fardo " mp-fardo.nr-fardo SKIP
           "Situacao " mp-fardo.situacao SKIP
           "Peso     " mp-fardo.peso SKIP
           "Peso Acumulado " de-peso
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ASSIGN i-num-etiqueta = 0.
END.
