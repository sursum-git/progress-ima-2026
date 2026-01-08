define temp-table tt-param no-undo
    FIELD  destino          as integer
    FIELD  arquivo          as char format "x(35)"
    FIELD  usuario          as char format "x(12)"
    FIELD  data-exec        as date
    FIELD  hora-exec        as integer
    FIELD  classifica       as integer
    FIELD  desc-classifica  as char format "x(40)"
    FIELD  modelo-rtf       as char format "x(35)"
    FIELD  l-habilitaRtf    as LOG
    FIELD  dt_ini_nf        AS DATE
    FIELD  dt_fim_nf        AS DATE
    FIELD  nr_pedido_ini    AS INT
    FIELD  nr_pedido_fim    AS INT
    .

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.
