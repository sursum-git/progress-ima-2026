
define temp-table tt-param
    field destino          as integer
    field arq-destino      as char
    field arq-entrada      as char
    field todos            as integer
    field usuario          as char
    field data-exec        as date
    field hora-exec        as integer.


DEFINE TEMP-TABLE tt-raw-digita NO-UNDO
    FIELD raw-digita	   AS RAW.

/* recebimento de parƒmetros */
DEFINE INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

output to "p:\prgapagarwp.log" append.

put substring(string(now),01,10)           format "x(10)" " as "
    substring(string(now),12,10)           format "x(08)" " - "
	"Apagando registro da tabela espec.wp" format "x(51)"
    skip.
for each espec.wp exclusive-lock:
    delete espec.wp.
end.

put substring(string(now),01,10)                     format "x(10)" " as "
    substring(string(now),12,10)                     format "x(08)" " - "
    "Apagando registro da tabela espec.wp_ped_venda" format "x(51)"
    skip.
for each espec.wp_ped_venda exclusive-lock:
    delete espec.wp_ped_venda.
end.

put substring(string(now),01,10)					   format "x(10)" " as "
    substring(string(now),12,10)					   format "x(08)" " - "
    "Apagando registro da tabela espec.wp_nota_fiscal" format "x(51)"
    skip.
for each espec.wp_nota_fiscal exclusive-lock:
    delete espec.wp_nota_fiscal.
end.

put substring(string(now),01,10)						 format "x(10)" " as "
    substring(string(now),12,10)						 format "x(08)" " - "
	"Apagando registro da tabela espec.wp_estoque_preco" format "x(51)"
    skip.
for each espec.wp_estoque_preco exclusive-lock:
    delete espec.wp_estoque_preco.
end.

put substring(string(now),01,10) format "x(10)" " as "
    substring(string(now),12,10) format "x(08)"
    skip.
put "----------------------------------------------------------------------------------------------" skip.

output close.
