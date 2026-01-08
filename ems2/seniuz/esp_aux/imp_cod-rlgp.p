DEF VAR i-tem AS INT.
DEF VAR i-ntem AS INT.

DEF STREAM saida1.
DEF STREAM saida2.

DEF TEMP-TABLE tt-work
    FIELD codigo   AS CHAR
    FIELD cod-rlgp AS CHAR
    INDEX ch-work codigo.
    
input from "c:/temp/Rec_lavagem.csv".
SET ^.

repeat:
   create tt-work.
   import delimiter ";" tt-work.
end.
input close.

OUTPUT STREAM saida1 TO c:/temp/ITEM_sem_rlgp.csv.
PUT STREAM saida1 "Item;Descricao;RLGP;Composicao" SKIP.

OUTPUT STREAM saida2 TO c:/temp/ITEM_alterado.csv.
PUT STREAM saida2 "Item;Descricao;RLGP-Antigo;RLGP_Novo;Composicao" SKIP.

FOR EACH ITEM WHERE ITEM.it-codigo >= "5"
                AND ITEM.it-codigo <= "5z"
                AND ITEM.ge-codigo >= 50
                AND ITEM.ge-codigo <= 59
              NO-LOCK.
    FIND FIRST tt-work WHERE tt-work.codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    FIND item-ext OF ITEM NO-ERROR.
    IF AVAIL tt-work THEN DO:
       IF AVAIL item-ext AND item-ext.cod-rlgp <> INT(tt-work.cod-rlgp) THEN DO:
          FIND composi WHERE composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.
          PUT STREAM saida2 UNFORMAT
              ITEM.it-codigo ";"
              ITEM.desc-item ";"
              item-ext.cod-rlgp ";"
              INT(tt-work.cod-rlgp) ";"
              IF AVAIL composi THEN composi.descricao
                               ELSE ";"
              SKIP.
          ASSIGN item-ext.cod-rlgp = INT(tt-work.cod-rlgp).
       END.
    END.
    ELSE DO:
       FIND composi WHERE composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.
       PUT STREAM saida1 UNFORMAT
           ITEM.it-codigo ";"
           ITEM.desc-item ";"
           item-ext.cod-rlgp ";" 
           IF AVAIL composi THEN composi.descricao
                            ELSE ";"
           SKIP.
    END.
END.
OUTPUT STREAM saida1 CLOSE.
OUTPUT STREAM saida2 CLOSE.
