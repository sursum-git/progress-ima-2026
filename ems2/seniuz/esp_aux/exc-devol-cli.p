/* Propgrama: exc-devol-cli.p
** Objetivo.: Gerar uma planilha excel com dados da tabela devol-cli
*/

DEF VAR i-lin AS INT INIT 2.

{utp/utapi003.i}

OS-DELETE VALUE("c:\temp\devol-cli.xls").
CREATE tt-configuracao.
ASSIGN tt-configuracao.versao-integracao   = 2
       tt-configuracao.arquivo-num         = 1
       tt-configuracao.arquivo             = "c:\temp\devol-cli.xls"
       tt-configuracao.total-planilha      = 1
       tt-configuracao.exibir-construcao   = yes
       tt-configuracao.abrir-excel-termino = YES.
       
CREATE tt-planilha.
ASSIGN tt-planilha.arquivo-num   = 1
       tt-planilha.planilha-num  = 1
       tt-planilha.planilha-nome = "Devol-cli"
       tt-planilha.linhas-grade  = no
       largura-coluna            = 8.50
       formatar-planilha         = yes.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 1
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Est".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 2
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Ser".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 3
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "NF".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 4
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "CodEmit".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 5
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "NomAbrev".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 6
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "SerDoc".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 7
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "NrDoc".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 8
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Dt-Devol".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 9
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Item".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 10
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Qt-devolvida".
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num   = 1
       tt-dados.planilha-num  = 1
       tt-dados.celula-coluna = 11
       tt-dados.celula-linha  = 1
       tt-dados.celula-valor  = "Vl.Devol".

FOR EACH devol-cli WHERE devol-cli.cod-estabel =  "2"
                     AND devol-cli.dt-devol    >= 06/01/2005
                     AND devol-cli.dt-devol    <= 06/30/2005
                   NO-LOCK:
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 1
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.cod-estabel.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 2
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.serie.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 3
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.nr-nota-fis.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 4
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = string(devol-cli.cod-emitente).
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 5
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.nome-ab-emi.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 6
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.serie-docto.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 7
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.nro-docto.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 8
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = string(devol-cli.dt-devol).
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 9
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "0"
           tt-dados.celula-alinhamento-horizontal = 4 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = devol-cli.it-codigo.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 10
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = string(devol-cli.qt-devolvida).
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num                   = 1
           tt-dados.planilha-num                  = 1
           tt-dados.celula-coluna                 = 11
           tt-dados.celula-linha                  = i-lin
           tt-dados.celula-formato                = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5 /* 1=Cent 4=Esq 5=Dir*/
           tt-dados.celula-valor                  = string(devol-cli.vl-devol).

    ASSIGN i-lin = i-lin + 1.
END.

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


