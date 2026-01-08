DEFINE VARIABLE iTam AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaior AS INTEGER     NO-UNDO.
DEFINE VARIABLE chist AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrPEdido AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt NO-UNDO
    FIELD nrPedido AS CHAR
    FIELD tamanho AS  INT
    INDEX primario IS PRIMARY nrpedido
    INDEX ind-tam tamanho DESC.

FOR EACH his-ped-venda-ext 
   WHERE nr-pedcli = '108679'
     :

    ASSIGN itam =  LENGTH(his-ped-venda-ext.ocorrencia).
    IF iTam > iMaior THEN DO:
       ASSIGN iMaior = iTam
              chist = ocorrencia
              nrPedido = his-ped-venda-ext.nr-pedcli.
    END.

    IF itam > 400 THEN DO:
       ASSIGN his-ped-venda-ext.ocorrencia =  substring(his-ped-venda-ext.ocorrencia,1,400).
       
    END.

       
    FIND tt
        WHERE tt.nrPedido  = his-ped-venda-ext.nr-pedcli NO-ERROR.
    IF NOT avail tt THEN DO:
        CREATE tt.
        ASSIGN tt.nrPedido = his-ped-venda-ext.nr-pedcli
               tt.tamanho  = iTam 
               .
    END.
    ELSE DO:
        IF iTam > tt.tamanho THEN
           ASSIGN tt.tamanho = itam.
    END.


END.

OUTPUT TO c:\temp\hist.txt.

FOR EACH tt
    USE-INDEX ind-tam.
    EXPORT DELIMITER "|" tt.
END.



OUTPUT CLOSE.


DISP imaior nrPedido.
MESSAGE chist VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
