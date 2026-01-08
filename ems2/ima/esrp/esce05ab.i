
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
    FIELD cod-estabel-ini  AS CHAR
    FIELD cod-estabel-fim  AS CHAR
    FIELD fi-it-codigo-ini AS CHAR
    FIELD fi-it-codigo-fim AS CHAR
    FIELD fi-cod-refer-ini AS CHAR
    FIELD fi-cod-refer-fim AS CHAR
    FIELD fi-ge-codigo-ini AS INT
    FIELD fi-ge-codigo-fim AS INT
    FIELD fi-dt-trans-ini  AS DATE
    FIELD fi-dt-trans-fim  AS DATE
    FIELD log-so-faturaveis AS LOGICAL
    FIELD niveldetalhe     AS INT
    .
