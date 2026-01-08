DEF VAR i-num-fardo     LIKE mp-fardo.nr-fardo.
DEF VAR l-ok AS LOG LABEL "GRAVA ? (S/N):" INITIAL YES.
REPEAT:
  CLEAR ALL.
  UPDATE i-num-fardo.
  
  FIND mp-fardo WHERE 
       mp-fardo.nr-fardo = i-num-fardo SHARE-LOCK NO-ERROR.
    
  IF NOT AVAIL mp-fardo THEN DO:
     MESSAGE "NÆo Achei o Fardo ! ! "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     UNDO,RETRY.
  END.
    

  DISP mp-fardo.nr-cdr     
       mp-fardo.situacao
       mp-fardo.dt-baixa
       mp-fardo.peso
       WITH SIDE-LABEL 1 COLUMN.


  UPDATE l-ok.
  IF l-ok = YES  THEN DO:
     ASSIGN i-num-fardo = 0.
     ASSIGN mp-fardo.situacao = 4
            mp-fardo.dt-baixa = TODAY.
  END.

END.
