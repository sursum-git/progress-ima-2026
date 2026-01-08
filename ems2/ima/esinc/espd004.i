define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD codEstabIni      AS CHAR
    FIELD codEstabFim      AS CHAR
    FIELD codRepIni        AS INT
    FIELD codRepFim        AS INT 
    FIELD mesIni           AS INT
    FIELD anoIni           AS INT
    FIELD mesFim           AS INT
    FIELD anoFIm           AS INT
                                         .
