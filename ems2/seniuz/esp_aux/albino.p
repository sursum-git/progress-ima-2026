DEFINE TEMP-TABLE tt-etq NO-UNDO 
       FIELD num-etiqueta     LIKE ob-etiqueta.num-etiqueta
       FIELD dt-emissao       LIKE ob-etiqueta.dt-emissao
       FIELD dt-trans         LIKE movto-etq.dt-trans
       FIELD nro-docto        LIKE movto-etq.nro-docto
       FIELD esp-docto        LIKE movto-etq.esp-docto 
       FIELD tipo-trans       AS CHAR FORMAT "x(3)"
       FIELD logtrans         AS CHAR FORMAT "x(80)"
       INDEX etq num-etiqueta.

OUTPUT TO PRINTER CONVERT TARGET "ISO8859-1" PAGED PAGE-SIZE 61.
PUT CONTROL "~033(s18H". 

/*
OUTPUT TO "c:\lixo\albino.txt".
*/

DEF VAR i-ant  AS INT.
DEF VAR i-nova AS INT.
DEF VAR i-pag  AS INT.
DEF VAR i-lin  AS INT.
DEF VAR c-tipo-trans AS CHAR.

FOR EACH ob-etiqueta WHERE ob-etiqueta.situacao   > 2 AND
                           ob-etiqueta.situacao   < 5 AND
                           ob-etiqueta.dt-emissao < 06/24/2007 NO-LOCK.
    FOR EACH movto-etq WHERE
             movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK.
        IF movto-etq.dt-trans = 06/26/2007 THEN DO.
           ASSIGN i-ant  = INT(INDEX(upper(movto-etq.char-1),"ANT"))
                  i-nova = INT(INDEX(upper(movto-etq.char-1),"NOVA")).
           IF  i-ant <> 0 AND i-nova <> 0 THEN DO.
              IF (INT(ENTRY(2,ENTRY(2,movto-etq.char-1,"Sit:"),":")) = 3  AND
                  INT(ENTRY(2,ENTRY(3,movto-etq.char-1,"Sit:"),":")) = 5)  OR
                 (INT(ENTRY(2,ENTRY(2,movto-etq.char-1,"Sit:"),":")) = 5  AND
                  INT(ENTRY(2,ENTRY(3,movto-etq.char-1,"Sit:"),":")) = 3) THEN DO.

                  CREATE tt-etq.
                  ASSIGN tt-etq.num-etiqueta = ob-etiqueta.num-etiqueta
                         tt-etq.dt-emissao   = ob-etiqueta.dt-emissao
                         tt-etq.dt-trans     = movto-etq.dt-trans
                         tt-etq.nro-docto    = movto-etq.nro-docto
                         tt-etq.esp-docto    = movto-etq.esp-docto
                         tt-etq.tipo-trans   = IF movto-etq.tipo-trans = YES THEN "ENT" ELSE "SAI"
                         tt-etq.logtrans     = SUBSTR(movto-etq.char-1,1,80).

              END.
           END.
        END.
    END.
END.
ASSIGN i-lin = 99
       i-pag = 1.
FOR EACH tt-etq NO-LOCK.

    IF i-lin > 61 THEN DO:
       RUN pi-imp-cabec.
       ASSIGN i-lin = 7
              i-pag = i-pag + 1.
    END.

    PUT  tt-etq.num-etiqueta            AT 1
         tt-etq.dt-emissao              AT 12
         tt-etq.dt-trans                AT 24
         tt-etq.nro-docto FORMAT "x(5)" AT 36
         tt-etq.esp-docto               AT 43
         tt-etq.tipo-trans              AT 47
         tt-etq.logtrans                AT 52.
    ASSIGN i-lin = i-lin + 1.
END.

PROCEDURE pi-imp-cabec.

  PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT   1
      "DATA: "                                  AT  66
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  72
      "HORA: "                                  AT  88
      STRING(TIME,"hh:mm:ss")                   AT  94
      "PAGINA:"                                 AT 116
      i-pag FORMAT "999"                        AT 124
      SKIP(1).
      
  PUT "RELATORIO DE TRANSACAO DA ETIQUETA" AT 51 SKIP(1).
                                                                                                                                
  PUT "Etiqueta   Dt.Emissao  Transacao   Docto  Esp Tipo Descricao da Transacao" AT 1.
  PUT "---------  ----------  ----------  -----  --- ---- ---------------------------------------------------------------------------" AT 1.

END.
