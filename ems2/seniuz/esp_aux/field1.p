FIND espec._File WHERE espec._File._File-name = 'ref-item-ext' EXCLUSIVE-LOCK.
FIND espec._Field WHERE espec._Field._File-recid = RECID(espec._File)
                    AND espec._Field._Field-name = 'cod-obsoleto'
                  EXCLUSIVE-LOCK.
DISP espec._File._File-name
     espec._Field._Field-name
     espec._Field._Initial
     WITH SIDE-LABELS 1 COLUMN.
