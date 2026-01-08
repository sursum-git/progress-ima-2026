/* Programa: imp-doc-fiscal3.p
** Objetivo: Importar acerto em documentos fiscais. 
*/

DEF VAR i-cont-err AS INT.

DEF TEMP-TABLE tt-cfop
    FIELD cfop-antigo  AS CHAR
    FIELD cfop-atual   AS CHAR
    INDEX ch-cfop cfop-antigo.
    
input from "c:/lixo/cfop.csv".
SET ^.

repeat:
   create tt-cfop.
   import delimiter ";" tt-cfop.
end.
input close.

FOR EACH doc-fiscal WHERE doc-fiscal.dt-docto >= 07/01/2002
                      AND doc-fiscal.dt-docto <= 12/31/2002
                    NO-LOCK.
    FIND tt-cfop WHERE tt-cfop.cfop-antigo = doc-fiscal.nat-operacao
                 NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-cfop THEN
       DISP doc-fiscal.nat-operacao.
END.
