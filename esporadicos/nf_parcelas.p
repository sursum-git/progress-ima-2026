DEFINE VARIABLE cNota AS CHARACTER FORMAT 'x(12)'   NO-UNDO.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE percComis AS DECIMAL     NO-UNDO.

DEFINE TEMP-TABLE tt
       FIELD data       AS DATE
       FIELD valor      AS DECIMAL
       FIELD valorComis AS DECIMAL 
       FIELD condicao   AS CHAR FORMAT 'x(200)'
       FIELD formula    AS CHAR
       /*FIELD vl_liquido  AS DECIMAL*/
       FIELD vl_comissao AS DECIMAL
       FIELD especie     AS CHAR
       FIELD dt_emissao  AS DATE
       FIELD nr_pedido   AS INT
       FIELD estab       AS CHAR
       FIELD conta       AS CHAR FORMAT 'x(15)'
       FIELD situacao    AS INT
       FIELD cod_vencto  AS INT
       FIELD nat_operacao AS CHAR
       FIELD nr_fatura    AS CHAR
       FIELD parcela      AS CHAR
       FIELD nome_cli     AS CHAR FORMAT 'x(12)'
       FIELD serie        AS CHAR.

UPDATE cnota LABEL "nota fiscal".
FOR EACH nota-fiscal
    WHERE nota-fiscal.cod-estabel = '5'
    AND nota-fiscal.serie = '3'
    AND nota-fiscal.nr-nota-fis = cNota NO-LOCK.
    DISP nota-fiscal.nr-pedcli.
    FIND FIRST ped-venda 
        WHERE int(nota-fiscal.nr-pedcli) = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    FIND FIRST ped-repre OF ped-venda
         WHERE ped-repre.nome-ab-rep <> 'fulano' NO-LOCK NO-ERROR.
    IF AVAIL ped-repre THEN
       ASSIGN percComis = ped-repre.perc-comis.
    ELSE
       ASSIGN percComis = 0.
    FIND FIRST cond-ped OF ped-venda NO-LOCK NO-ERROR.
    IF NOT AVAIL cond-ped THEN DO:
       FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR.
       IF AVAIL cond-pagto THEN DO:
          REPEAT iCont = 1 TO cond-pagto.num-parcelas:
          CREATE tt.
          
          ASSIGN tt.valor       = nota-fiscal.vl-tot-nota * per-pg-dup[icont] / 100
                 tt.data        = nota-fiscal.dt-emis-nota + prazos[iCont]
                 tt.condicao    = cond-pagto.descricao
                 tt.formula     = 'f204'
                 tt.valorComis  =  tt.valor * (100 - percComis) / 100
                 tt.especie     = 'dp'
                 tt.dt_emissao  = nota-fiscal.dt-emis-nota
                 tt.nr_pedido   = IF AVAIL ped-venda THEN ped-venda.nr-pedido ELSE 0
                 tt.estab       = nota-fiscal.cod-estabel
                 tt.conta       = '19000006'
                 tt.situacao    = 1
                 tt.cod_vencto  = 1
                tt.nat_operacao = nota-fiscal.nat-operacao
                tt.nr_fatura    = nota-fiscal.nr-nota-fis
                 tt.parcela      = IF iCont > 10 THEN STRING(iCont) ELSE '0' + string(iCont)
                 tt.nome_cli     = nota-fiscal.nome-ab-cli
                 tt.serie        = '3' .
          END.                                                       
       END.
    END.
    ELSE DO:
       FOR EACH cond-ped OF ped-venda:
           CREATE tt.
           ASSIGN tt.valor = nota-fiscal.vl-tot-nota  * cond-ped.perc-pagto / 100
                  tt.data  = nota-fiscal.dt-emis-nota + nr-dias-venc
                  tt.formula     = 'f204'
                  tt.valorComis  =  tt.valor * (100 - percComis) / 100
                  tt.especie     = 'dp'
                  tt.dt_emissao  = nota-fiscal.dt-emis-nota
                  tt.nr_pedido   = IF AVAIL ped-venda THEN ped-venda.nr-pedido ELSE 0
                  tt.estab       = nota-fiscal.cod-estabel
                  tt.conta       = '19000006'
                  tt.situacao    = 1
                  tt.cod_vencto  = 1
                  tt.nat_operacao = nota-fiscal.nat-operacao
                  tt.nr_fatura    = nota-fiscal.nr-nota-fis
                  tt.parcela      = IF iCont > 10 THEN STRING(iCont) ELSE '0' + string(iCont)
                  tt.nome_cli     = nota-fiscal.nome-ab-cli
                  tt.serie        = '3' .
       END.
    END.
    MESSAGE 'nao achou o pedido de venda'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF NOT AVAIL ped-venda THEN DO:
       FIND cond-pagto OF nota-fiscal NO-LOCK NO-ERROR.
       IF AVAIL cond-pagto THEN DO:
          MESSAGE 'vai buscar pela condi‡Æo de pagto da nf'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          REPEAT iCont = 1 TO cond-pagto.num-parcelas:
              CREATE tt.
              ASSIGN tt.valor       = nota-fiscal.vl-tot-nota * per-pg-dup[icont] / 100
                     tt.data        = nota-fiscal.dt-emis-nota + prazos[iCont]
                     tt.condicao    = cond-pagto.descricao
                     tt.formula     = 'f204'
                     tt.valorComis  =  tt.valor * (100 - percComis) / 100
                     tt.especie     = 'dp'
                     tt.dt_emissao  = nota-fiscal.dt-emis-nota
                     tt.nr_pedido   = IF AVAIL ped-venda THEN ped-venda.nr-pedido ELSE 0
                     tt.estab       = nota-fiscal.cod-estabel
                     tt.conta       = '19000006'
                     tt.situacao    = 1
                     tt.cod_vencto  = 1
                     tt.nat_operacao = nota-fiscal.nat-operacao
                     tt.nr_fatura    = nota-fiscal.nr-nota-fis
                     tt.parcela      = IF iCont > 10 THEN STRING(iCont) ELSE '0' + string(iCont)
                     tt.nome_cli     = nota-fiscal.nome-ab-cli
                     tt.serie        = '3' .
          END.                                                       
       END.
       ELSE DO:
           MESSAGE 'nota fiscal sem pedido e sem condi‡Æo de pagamento cadastrada.'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
    END.
    OUTPUT TO value('c:\temp\fatu_duplic_' + NOTA-FISCAL.NR-NOTA-FIS + "_" +  STRING(TIME) + '.TXT').
    FOR EACH fat-duplic 
        WHERE fat-duplic.nr-fatura =  nota-fiscal.nr-fatura:
        //DISP fat-duplic WITH 1 COL WIDTH 550.
        EXPORT fat-duplic.
        DELETE fat-duplic.
    END.
    OUTPUT CLOSE.


END.

FOR EACH tt:
    IF WEEKDAY(tt.data) = 7 THEN
       ASSIGN tt.data = tt.data + 2.
    IF WEEKDAY(tt.data) = 1 THEN
       ASSIGN tt.data = tt.data + 1.
                                   
    DISP tt WITH 1 COL WIDTH 550.
END.

FOR EACH tt:
    CREATE fat-duplic.
    ASSIGN fat-duplic.serie         = tt.serie
           fat-duplic.nome-ab-cli   = tt.nome_cli
           fat-duplic.parcela       = tt.parcela
           fat-duplic.nr-fatura     = tt.nr_fatura
           fat-duplic.dt-venciment  = tt.data
           fat-duplic.vl-parcela    = tt.valor
           fat-duplic.nat-operacao  = tt.nat_operacao
           fat-duplic.cod-vencto    = tt.cod_vencto
           fat-duplic.ind-fat-nota  = tt.situacao
           fat-duplic.ct-recven     = tt.conta
           fat-duplic.cod-estabel   = tt.estab
           fat-duplic.vl-comis      = tt.valorComis
           fat-duplic.nr-pedido     = tt.nr_pedido 
           fat-duplic.dt-emissao    = tt.dt_emissao
           fat-duplic.cod-esp       = tt.especie 
           fat-duplic.dec-1         = tt.valor
           fat-duplic.vl-comis-me   = tt.valorComis
           fat-duplic.vl-parcela-me = tt.valor
           fat-duplic.cod-formula   = 'f204'.
END.
MESSAGE 'faturas criadas com sucesso'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
