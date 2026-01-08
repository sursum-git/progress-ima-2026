DEFINE STREAM sPedido.
DEFINE STREAM sItem.
DEFINE STREAM sCondicao.
DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO FORMAT 'x(2000)'.
DEFINE TEMP-TABLE tt
    FIELD linha AS CHAR FORMAT 'x(2000)'.
OUTPUT STREAM sPedido   TO c:\temp\ped-venda.txt.
OUTPUT STREAM sItem     TO c:\temp\it-ped-venda.txt.
OUTPUT STREAM sCondicao TO c:\temp\condicao.txt.
FOR EACH ped-venda NO-LOCK
    WHERE YEAR(ped-venda.dt-emissao) = 2016.
    EXPORT STREAM sPedido DELIMITER "|" ped-venda.
    FOR EACH ped-item NO-LOCK 
        OF ped-venda.
        EXPORT STREAM sitem DELIMITER "|" ped-venda.
    END.
    FOR EACH cond-ped OF ped-venda NO-LOCK.
        EXPORT STREAM sCondicao DELIMITER "|" cond-ped.
    END.
END.
OUTPUT STREAM sPedido   CLOSE.
OUTPUT STREAM sItem     CLOSE.
OUTPUT STREAM sCondicao CLOSE.

/*INPUT FROM c:\temp\ped-venda.txt.
REPEAT:
  IMPORT UNFORM cLinha.
  ASSIGN cLinha = REPLACE(clinha,CHR(10) + CHR(13),'').
  CREATE tt.
  ASSIGN tt.linha = cLinha.

END.
INPUT CLOSE.

OUTPUT TO c:\temp\ped-venda-conv.txt.
   FOR EACH tt:
       PUT tt.linha SKIP.
   END.

OUTPUT CLOSE.

EMPTY TEMP-TABLE tt.
INPUT FROM c:\temp\it-ped-venda.txt.
REPEAT:
  IMPORT UNFORM cLinha.
  ASSIGN cLinha = REPLACE(clinha,CHR(10) + CHR(13),'').
  CREATE tt.
  ASSIGN tt.linha = cLinha.

END.
INPUT CLOSE.

OUTPUT TO c:\temp\it-venda-conv.txt.
   FOR EACH tt:
       PUT tt.linha SKIP.
   END.

OUTPUT CLOSE.


EMPTY TEMP-TABLE tt.
INPUT FROM c:\temp\condicao.txt.
REPEAT:
  IMPORT UNFORM cLinha.
  ASSIGN cLinha = REPLACE(clinha,CHR(10) + CHR(13),'').
  CREATE tt.
  ASSIGN tt.linha = cLinha.

END.
INPUT CLOSE.

OUTPUT TO c:\temp\condicao-conv.txt.
   FOR EACH tt:
       PUT tt.linha SKIP.
   END.

OUTPUT CLOSE.*/

