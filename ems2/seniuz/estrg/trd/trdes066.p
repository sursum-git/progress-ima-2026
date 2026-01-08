TRIGGER PROCEDURE FOR DELETE OF mp-mistura.

FOR EACH mp-comp-mistura OF mp-mistura EXCLUSIVE-LOCK.
    DELETE mp-comp-mistura.
END.

FOR EACH mp-distribuicao OF mp-mistura EXCLUSIVE-LOCK.
    DELETE mp-distribuicao.
END.
