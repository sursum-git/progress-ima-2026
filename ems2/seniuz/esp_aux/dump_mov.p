FOR EACH mgmov._file WHERE mgmov._file._file-name <> "movto-estoq"
                       AND mgmov._file._file-name <> "movto-mat"
                       AND mgmov._file._Hidden = NO.
    RUN prodict/dump_d.p (INPUT mgmov._file._file-name, INPUT "/tmp/dump_ems/mov", INPUT "ibm850").
END.
QUIT.

