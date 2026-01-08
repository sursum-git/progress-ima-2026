FOR EACH mgcad._file WHERE mgcad._file._Hidden = NO 
                     NO-LOCK:
    RUN prodict/dump_d.p (input mgcad._file._File-Name, input "U:/Lixo", input "ibm850").
END.

