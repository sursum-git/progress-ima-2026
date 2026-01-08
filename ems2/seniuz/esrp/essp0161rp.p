DEF INPUT PARAMETER p-nr-nota-ini LIKE nota-fiscal.nr-nota-fis.
DEF INPUT PARAMETER p-nr-nota-fin LIKE nota-fiscal.nr-nota-fis.
DEF INPUT PARAMETER p-cod-estabel LIKE nota-fiscal.cod-estabel.
DEF INPUT PARAMETER p-serie       LIKE nota-fiscal.serie.

 /* Imprime a Nota Fiscal */
FIND ser-estab WHERE
     ser-estab.serie = p-serie AND
     ser-estab.cod-estab = p-cod-estabel NO-LOCK NO-ERROR.

IF SUBSTR(ser-estab.char-1,1,4) = 'yes1' THEN /* Danfe Modelo 1 */
   RUN esrp/essp0161rp-a.p (INPUT p-nr-nota-ini, 
                            INPUT p-nr-nota-fin,
                            INPUT p-cod-estabel,
                            INPUT p-serie).
ELSE
IF SUBSTR(ser-estab.char-1,1,4) = 'yes3' THEN /* Danfe Modelo 3 */
   RUN esrp/essp0161rp-b.p (INPUT p-nr-nota-ini, 
                            INPUT p-nr-nota-fin,
                            INPUT p-cod-estabel,
                            INPUT p-serie).


