/* Programa: ret-udm.p
   Funcao..: Retornar o Ultimo dia de um determinado Mˆs
   Autor...: SeniuZ - Toninho    Agosto/2005 
*/

DEF INPUT PARAMETER p-periodo AS CHAR.
DEF OUTPUT PARAMETER p-dia AS CHAR.
DEF VAR i-mes AS INT.
DEF VAR i-ano AS INT.
DEF VAR p-dias AS CHAR EXTENT 12 INIT [31,28,31,30,31,30,31,31,30,31,30,31].

IF LENGTH(p-periodo) <> 6 THEN DO.
   MESSAGE 'Periodo deve ser informado no formato ("999999")'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ASSIGN p-dia = '0'.
END.
ELSE DO.
    ASSIGN i-mes = INT(SUBSTR(p-periodo,1,2))
           i-ano = INT(SUBSTR(p-periodo,3,4)) 
           p-dia = p-dias[i-mes].
    
    IF i-mes = 2 THEN DO.
       IF i-ano / 4 = INT(i-ano / 4) THEN
          ASSIGN p-dia = '29'.
    END.
END.



