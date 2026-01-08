FOR EACH espec._file WHERE espec._file._Hidden = NO 
                     NO-LOCK:
    RUN prodict/dump_d.p (input espec._file._File-Name, input "R:", input "ibm850").
END.

