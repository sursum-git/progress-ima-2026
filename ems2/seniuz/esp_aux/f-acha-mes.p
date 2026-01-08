DEF VAR c-per AS CHAR FORMAT "x(8)" EXTENT 12.
DEF VAR c-per-aux AS CHAR FORMAT "x(108)".
DEF VAR da-data-inf AS DATE.
DEF VAR i-cont AS INT.

FUNCTION f-acha-mes RETURNS INTEGER (INPUT da-data AS DATE).
   DEFINE VARIABLE c-data   AS CHARACTER  FORMAT "x(8)".
   DEFINE VARIABLE i-indice AS INTEGER.

   ASSIGN c-data = SUBSTR("JanFevMarAbrMaiJunJulAgoSetOutNovDez",MONTH(da-data) * 3 - 2, 3) +
                   "/" + STRING(YEAR(da-data),"9999").
   i-indice = LOOKUP(c-data,c-per-aux).
   RETURN i-indice.
END FUNCTION.

ASSIGN c-per[01] = "Ago/2005"
       c-per[02] = "Set/2005"
       c-per[03] = "Out/2005"
       c-per[04] = "Nov/2005"
       c-per[05] = "Dez/2005"
       c-per[06] = "Jan/2006"
       c-per[07] = "Fev/2006"
       c-per[08] = "Mar/2006"
       c-per[09] = "Abr/2006"
       c-per[10] = "Mai/2006"
       c-per[11] = "Jun/2006"
       c-per[12] = "Jul/2006".
DO i-cont = 1 TO 12:
   ASSIGN c-per-aux = c-per-aux + c-per[i-cont] + ",".
END.

REPEAT:
   UPDATE da-data-inf.
   DISP f-acha-mes(da-data-inf).
END.
