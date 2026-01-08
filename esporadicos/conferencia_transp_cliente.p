DEFINE VARIABLE dtEmisNota   AS DATE   NO-UNDO.
DEFINE VARIABLE nrNotaFis    AS CHAR    NO-UNDO.
DEFINE VARIABLE nomeTransp   AS CHARACTER   NO-UNDO.
OUTPUT TO c:\temp\emitente_x_transp.txt.
FOR EACH emitente
    WHERE identific = 1
    OR identific = 3.
    FIND FIRST transporte 
        WHERE transporte.cod-transp =  emitente.cod-transp 
        NO-LOCK NO-ERROR .
    FIND LAST nota-fiscal
        OF emitente NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN dtEmisNota = nota-fiscal.dt-emis-nota
              nrNotaFis  = nota-fiscal.nr-nota-fis
              nomeTransp = nota-fiscal.nome-transp.
    ELSE
        ASSIGN dtEmisNota = ?
               nrNotaFis  = ''
               nomeTransp = ''.
    
    EXPORT DELIMITER "|" emitente.cod-emitente emitente.nome-abrev
        emitente.cod-transp transporte.nome emitente.cidade emitente.estado 
        emitente.data-implant 
        dtEmisNota
        nrNotaFis
        nomeTransp.

    



END.
OUTPUT CLOSE.
