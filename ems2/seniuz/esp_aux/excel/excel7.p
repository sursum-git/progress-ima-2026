/*
   Programa Exemplo para Uso na Gera‡Æo de Planilha
            pelo Progress.


*/


DEF VAR de-totais AS DEC EXTENT 6.

{utp/utapi003.i}


OS-DELETE VALUE("c:\temp\Vendas & Faturamento.xls").
CREATE tt-configuracao.
ASSIGN tt-configuracao.versao-integracao   = 2
       tt-configuracao.arquivo-num         = 1
       tt-configuracao.arquivo             = "c:\temp\Vendas & Faturamento.xls"
       tt-configuracao.total-planilha      = 1
       tt-configuracao.exibir-construcao   = YES
       tt-configuracao.abrir-excel-termino = YES.
       
CREATE tt-planilha.
ASSIGN tt-planilha.arquivo-num       = 1
       tt-planilha.planilha-num      = 1
       tt-planilha.planilha-nome     = "Dados"
       tt-planilha.formatar-planilha = YES
       tt-planilha.linhas-grade      = NO 
       tt-planilha.largura-coluna    = 17.

/* Titulo da Planilha */
/*                    */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num          = 1
       tt-dados.planilha-num         = 1
       tt-dados.celula-coluna        = 1
       tt-dados.celula-linha         = 1
       tt-dados.celula-fonte-nome    = "verdana"
       tt-dados.celula-fonte-tamanho = 13
       tt-dados.celula-fonte-negrito = YES
       tt-dados.celula-fonte-cor     = 3 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior  = 2 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-valor         = "Tear Textil Industria e Comercio Ltda".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num          = 1
       tt-dados.planilha-num         = 1
       tt-dados.celula-coluna        = 5
       tt-dados.celula-fonte-nome    = "verdana"
       tt-dados.celula-fonte-tamanho = 13
       tt-dados.celula-fonte-negrito = YES
       tt-dados.celula-fonte-cor     = 3 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior  = 2 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-linha         = 1
       tt-dados.celula-valor         = "VENDAS E FATURAMENTO DO PERIODO 05/2008".

/* Cabe‡alho da Planilha */
/*                       */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 1
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 1 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "DIAS". 

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "Metros Vendidos".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 3
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "Valores Vendidos".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 4
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "Metros Cancelados".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 5
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "Valores Cancelados".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 6
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "Metros Faturados".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 7
       tt-dados.celula-linha                  = 3
       tt-dados.celula-fonte-nome             = "Lucida Console"
       tt-dados.celula-fonte-tamanho          = 13
       tt-dados.celula-fonte-negrito          = YES
       tt-dados.celula-fonte-cor              = 5 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "Valores Faturados".

/* INCLUSÇO DAS LINHAS DE DADOOS DA PLANILHA */
/*                                           */
/* Linha 4 */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 1
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 1 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "2".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "92024.50".

ASSIGN de-totais[1] = de-totais[1] + 92024.5.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 3
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "493508.14".

ASSIGN de-totais[2] = de-totais[2] + 493508.14.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 4
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "24236.85".

ASSIGN de-totais[3] = de-totais[3] + 24236.85.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 5
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "85044.67".

ASSIGN de-totais[4] = de-totais[4] + 85044.67.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 6
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "11247.45".

ASSIGN de-totais[5] = de-totais[5] + 11247.45.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 7
       tt-dados.celula-linha                  = 4
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "65081.67".

ASSIGN de-totais[6] = de-totais[6] + 65081.14.

/* Linha 5 */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 1
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 1 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor  = "5".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "18509.30".

ASSIGN de-totais[1] = de-totais[1] + 18509.30.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 3
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "80492.18".

ASSIGN de-totais[2] = de-totais[2] + 80492.18.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 4
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "36007.00".

ASSIGN de-totais[3] = de-totais[3] + 36007.14.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 5
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "104978.01".

ASSIGN de-totais[4] = de-totais[4] + 104978.01.


CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 6
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "46915.90".

ASSIGN de-totais[5] = de-totais[5] + 46915.90.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 7
       tt-dados.celula-linha                  = 5
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "246360.15".

ASSIGN de-totais[6] = de-totais[6] + 246360.15.

/* Linha 6 */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 1
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 1 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "6".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "66313.55".

ASSIGN de-totais[1] = de-totais[1] + 66313.55.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 3
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "215250.49".

ASSIGN de-totais[2] = de-totais[2] + 215250.49.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 4
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "26875.40".

ASSIGN de-totais[3] = de-totais[3] + 26875.40.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 5
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "141094.80".

ASSIGN de-totais[4] = de-totais[4] + 141094.80.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 6
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "44037.20".

ASSIGN de-totais[5] = de-totais[5] + 44037.20.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 7
       tt-dados.celula-linha                  = 6
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "249693.81".

ASSIGN de-totais[6] = de-totais[6] + 249693.81.

/* Linha 7 */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 1
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 1 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "7".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "60486.90".

ASSIGN de-totais[1] = de-totais[1] + 60486.90.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 3
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "334370.74".

ASSIGN de-totais[2] = de-totais[2] + 334370.74.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 4
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "2060.80".

ASSIGN de-totais[3] = de-totais[3] + 2060.80.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 5
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "11901.04".

ASSIGN de-totais[4] = de-totais[4] + 11901.04.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 6
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "53220.70".

ASSIGN de-totais[5] = de-totais[5] + 53220.70.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 7
       tt-dados.celula-linha                  = 7
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "269950.65".

ASSIGN de-totais[6] = de-totais[6] + 269951.65.

/* Linha 8 */
CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 1
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 1 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "8".

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 2
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "51311.70".

ASSIGN de-totais[1] = de-totais[1] + 51311.70.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 3
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00"
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "261135.40".

ASSIGN de-totais[2] = de-totais[2] + 261134.40.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 4
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "13045.80".

ASSIGN de-totais[3] = de-totais[3] + 13045.80.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 5
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "67150.94".

ASSIGN de-totais[4] = de-totais[4] + 67150.94.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 6
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 3 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "28572.80".

ASSIGN de-totais[5] = de-totais[5] + 28572.80.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = 1
       tt-dados.planilha-num                  = 1
       tt-dados.celula-coluna                 = 7
       tt-dados.celula-linha                  = 8
       tt-dados.celula-fonte-nome             = "Courier New"
       tt-dados.celula-fonte-tamanho          = 12
       tt-dados.celula-fonte-negrito          = NO
       tt-dados.celula-fonte-italico          = YES
       tt-dados.celula-fonte-sublinhado       = 3  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = 50 /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = 2
       tt-dados.celula-formato                = "###.###.##0,00" 
       tt-dados.celula-alinhamento-vertical   = 4 /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = 5 /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = "145769.24".

ASSIGN de-totais[6] = de-totais[6] + 145769.24.


RUN pi-grava-linha (INPUT 1,
                    INPUT 1,
                    INPUT 2,
                    INPUT 10,
                    INPUT "Batang",
                    INPUT 12,
                    INPUT NO,
                    INPUT YES,
                    INPUT 3,
                    INPUT 54,
                    INPUT 2,
                    INPUT "###.###.##0,00",
                    INPUT 4,
                    INPUT 5,
                    INPUT STRING(de-totais[1])).


RUN pi-grava-linha (INPUT 1,
                    INPUT 1,
                    INPUT 3,
                    INPUT 10,
                    INPUT "Batang",
                    INPUT 12,
                    INPUT NO,
                    INPUT YES,
                    INPUT 3,
                    INPUT 54,
                    INPUT 2,
                    INPUT "###.###.##0,00",
                    INPUT 4,
                    INPUT 5,
                    INPUT STRING(de-totais[2])).

RUN pi-grava-linha (INPUT 1,
                    INPUT 1,
                    INPUT 4,
                    INPUT 10,
                    INPUT "Batang",
                    INPUT 12,
                    INPUT NO,
                    INPUT YES,
                    INPUT 3,
                    INPUT 54,
                    INPUT 2,
                    INPUT "###.###.##0,00",
                    INPUT 4,
                    INPUT 5,
                    INPUT STRING(de-totais[3])).

RUN pi-grava-linha (INPUT 1,
                    INPUT 1,
                    INPUT 5,
                    INPUT 10,
                    INPUT "Batang",
                    INPUT 12,
                    INPUT NO,
                    INPUT YES,
                    INPUT 3,
                    INPUT 54,
                    INPUT 2,
                    INPUT "###.###.##0,00",
                    INPUT 4,
                    INPUT 5,
                    INPUT STRING(de-totais[4])).

RUN pi-grava-linha (INPUT 1,
                    INPUT 1,
                    INPUT 6,
                    INPUT 10,
                    INPUT "Batang",
                    INPUT 12,
                    INPUT NO,
                    INPUT YES,
                    INPUT 3,
                    INPUT 54,
                    INPUT 2,
                    INPUT "###.###.##0,00",
                    INPUT 4,
                    INPUT 5,
                    INPUT STRING(de-totais[5])).

RUN pi-grava-linha (INPUT 1,
                    INPUT 1,
                    INPUT 7,
                    INPUT 10,
                    INPUT "Batang",
                    INPUT 12,
                    INPUT NO,
                    INPUT YES,
                    INPUT 3,
                    INPUT 54,
                    INPUT 2,
                    INPUT "###.###.##0,00",
                    INPUT 4,
                    INPUT 5,
                    INPUT STRING(de-totais[6])).

/*
CREATE tt-grafico.
ASSIGN tt-grafico.arquivo-num = 1
       tt-grafico.planilha-num = 1
       tt-grafico.grafico-nome = "Graficos"
       tt-grafico.grafico-titulo = "QUANTIDADE VENDIDAS"
       tt-grafico.grafico-tipo = 15
       tt-grafico.intervalo-linha-ini = 4
       tt-grafico.intervalo-linha-fin = 10
       tt-grafico.intervalo-coluna-ini = 2
       tt-grafico.intervalo-coluna-fin = 3.

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


/* P R O C E D I M E N T O S */
/*                           */
PROCEDURE pi-grava-linha.

 DEF INPUT PARAMETER p-arquivo-num                   AS INT.
 DEF INPUT PARAMETER p-planilha-num                  AS INT.
 DEF INPUT PARAMETER p-celula-coluna                 AS INT.
 DEF INPUT PARAMETER p-celula-linha                  AS INT.
 DEF INPUT PARAMETER p-celula-fonte-nome             AS CHAR.
 DEF INPUT PARAMETER p-celula-fonte-tamanho          AS INT.
 DEF INPUT PARAMETER p-celula-fonte-negrito          AS LOG.
 DEF INPUT PARAMETER p-celula-fonte-italico          AS LOG.
 DEF INPUT PARAMETER p-celula-fonte-sublinhado       AS INT.
 DEF INPUT PARAMETER p-celula-fonte-cor              AS INT.
 DEF INPUT PARAMETER p-celula-cor-interior           AS INT.
 DEF INPUT PARAMETER p-celula-formato                AS CHAR.
 DEF INPUT PARAMETER p-celula-alinhamento-vertical   AS INT.
 DEF INPUT PARAMETER p-celula-alinhamento-horizontal AS INT.
 DEF INPUT PARAMETER p-celula-valor                  AS CHAR.

CREATE tt-dados.
ASSIGN tt-dados.arquivo-num                   = p-arquivo-num
       tt-dados.planilha-num                  = p-planilha-num
       tt-dados.celula-coluna                 = p-celula-coluna
       tt-dados.celula-linha                  = p-celula-linha
       tt-dados.celula-fonte-nome             = p-celula-fonte-nome
       tt-dados.celula-fonte-tamanho          = p-celula-fonte-tamanho
       tt-dados.celula-fonte-negrito          = p-celula-fonte-negrito
       tt-dados.celula-fonte-italico          = p-celula-fonte-italico
       tt-dados.celula-fonte-sublinhado       = p-celula-fonte-sublinhado  /* 1=Duplo 2=Contabil Duplo 3=Nenhum 4=Simples 5=Contabil Simples */
       tt-dados.celula-fonte-cor              = p-celula-fonte-cor /* 1=Preto 2=Branco 3=Vermelho 4=Verde 5=Azul 6=Amarelho .... */
       tt-dados.celula-cor-interior           = p-celula-cor-interior
       tt-dados.celula-formato                = p-celula-formato
       tt-dados.celula-alinhamento-vertical   = p-celula-alinhamento-vertical /* 1=Abaixo 2=Centralizado 3=Distribuido 4=Justificado 5=Acima */
       tt-dados.celula-alinhamento-horizontal = p-celula-alinhamento-horizontal /* 1=Centralizado 2=Distribuido 3=Justificado 4=Esquerda 5=Direita */
       tt-dados.celula-valor                  = p-celula-valor.

END PROCEDURE.


