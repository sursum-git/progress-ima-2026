{utp/utapi003.i}

OS-DELETE VALUE("c:\temp\teste_utapi003.xls").
CREATE tt-configuracao.
ASSIGN tt-configuracao.versao-integracao   = 2
       tt-configuracao.arquivo-num         = 1
       tt-configuracao.arquivo             = "c:\temp\teste_utapi003.xls"
       tt-configuracao.total-planilha      = 1
       tt-configuracao.exibir-construcao   = yes
       tt-configuracao.abrir-excel-termino = YES.
       
CREATE tt-planilha.
ASSIGN tt-planilha.arquivo-num   = 1
       tt-planilha.planilha-num  = 1
       tt-planilha.planilha-nome = "Plan 1 - Grafico 1"
       tt-planilha.linhas-grade  = no
       largura-coluna            = 8.50
       formatar-planilha         = yes.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Regi∆o".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 2
       tt-dados.celula-valor  = "Norte".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 3
       tt-dados.celula-valor  = "Nordeste".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 4
       tt-dados.celula-valor  = "Sul".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 5
       tt-dados.celula-valor  = "Sudeste".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 6
       tt-dados.celula-valor  = "Centro-Oeste".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 2
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Valor 1".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Cent 4=Esq 5=Dir*/
       tt-dados.celula-valor                  = "123456789,01".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 2
       tt-dados.celula-linha  = 3
       tt-dados.celula-valor  = "20".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 2
       tt-dados.celula-linha  = 4
       tt-dados.celula-valor  = "30".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 2
       tt-dados.celula-linha  = 5
       tt-dados.celula-valor  = "40".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 2
       tt-dados.celula-linha  = 6
       tt-dados.celula-valor  = "50".
/*
CREATE tt-grafico.
ASSIGN tt-grafico.arquivo-num = 1
       tt-grafico.planilha-num = 1
       tt-grafico.grafico-nome = "Graf 1"
       tt-grafico.grafico-titulo = "Valores (Plan 1)"
       tt-grafico.grafico-tipo = 4
       tt-grafico.intervalo-linha-ini = 1
       tt-grafico.intervalo-linha-fin = 6
       tt-grafico.intervalo-coluna-ini = 1
       tt-grafico.intervalo-coluna-fin = 2.
*/

RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                    INPUT-OUTPUT TABLE tt-planilha,
                    INPUT-OUTPUT TABLE tt-dados,
                    INPUT-OUTPUT TABLE tt-grafico,
                    INPUT-OUTPUT TABLE tt-erros).
if return-value = "nok" then do: 
    for each tt-erros: 
        disp tt-erros with 1 col width 500. 
    end.
end.                  


