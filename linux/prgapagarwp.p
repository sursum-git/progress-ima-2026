output to "/log/prgapagarwp.log" append.

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

put substring(string(now),01,10)						 format "x(10)" " as "
    substring(string(now),12,10)						 format "x(08)" " - "
	"--------------------------------------------------" format "x(51)"
    skip.

output close.