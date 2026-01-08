//NotaRemessa
DEFINE VARIABLE cContainer AS CHARACTER   NO-UNDO.
UPDATE cContainer.
CREATE  lisa-integra.
ASSIGN lisa-integra.cod-trans = 'NotaRemessa'
       lisa-integra.chave = cContainer
       lisa-integra.acao = 'GERAR'
       lisa-integra.ind-situacao = 1
       .

