DEFINE VARIABLE iRecid      AS INT64     NO-UNDO.
DEFINE VARIABLE iPartition  AS INTEGER     NO-UNDO.

{esp/convRowidRecid.i}

FIND FIRST emitente NO-LOCK NO-ERROR.



ASSIGN iRecid = HexToInt(string(ROWID(emitente)), OUTPUT iPartition).

UPDATE iRecid .

MESSAGE ROWID(emitente) SKIP
        iRecid SKIP
        iPartition
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
