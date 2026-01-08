FOR EACH espec._file WHERE
         espec._file._dump-name <> ?
    BY espec._file._dump-name DESCENDING.
    DISP espec._file._dump-name.
END.
