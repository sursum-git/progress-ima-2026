DEFINE VARIABLE hBoConsDin          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMD               AS HANDLE      NO-UNDO.
DEFINE VARIABLE cListaCampos        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTabela             AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
{util.i}
{esbo/boMetaDados.i}
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD tabela AS CHAR FORMAT 'x(30)'.
RUN esbo/boConsdin.p    PERSIST SET hBoConsDin.
RUN esbo/boMetaDados.p  PERSIST SET hBoMD.
RUN iniciarBos IN hBoConsDin.

/*INPUT FROM c:\temp\kunumi\tbs13.csv.
REPEAT:
    CREATE tt. 
    IMPORT  tt.

END.*/


CREATE tt. 
ASSIGN tt.tabela = 'item'.

INPUT CLOSE.
FOR EACH tt
    WHERE tt.tabela <> '':
    DISP tt.tabela. PAUSE 0.
    RUN limparFiltros IN hBoMD.
    RUN setTabela IN hBoMD(tt.tabela).
    IF tt.tabela = 'cliente' OR tt.tabela = 'fornecedor' THEN
       RUN setBanco IN hBoMd('ems5').
    RUN limparTTCampos IN hBoMD.
    RUN getCpsTb IN hBoMD.
    RUN getTTCps IN hBoMD(OUTPUT TABLE ttCampos).
    
    
    RUN extrairListaCpsTb(TEMP-TABLE ttCampos:DEFAULT-BUFFER-HANDLE,
                                  'nome',
                                  '',
                                  OUTPUT cListaCampos).

   
   // emitente ASSIGN cListaCampos = "cod-emitente,nome-abrev,nome-emit,atividade,bairro,categoria,cep,cgc,cidade,cod-canal-venda,cod-classif-cliente,cod-gr-cli,cod-rep,data-implant,e-mail,endereco,estado,identific,nome-matriz".
   //familia ASSIGN  cListaCampos = "fm-codigo,descricao".
   // it-nota-fisc ASSIGN cListaCampos = "cod-estabel,serie,nr-nota-fis,nr-seq-fat,it-codigo,cod-refer,peso-liq-fat,peso-bruto,qt-faturada[1],un-fatur[1],vl-pretab,vl-preori,vl-preuni,vl-merc-tab,vl-merc-ori,vl-merc-liq,vl-tot-item,cod-depos,nat-operacao,nr-seq-ped,nr-pedcli,nome-ab-cli".
   // nota-fiscal ASSIGN cListaCampos = "cod-estabel,serie,nr-nota-fis,nome-ab-cli, no-ab-reppri, vl-tot-nota,dt-emis-nota,dt-cancela,ind-sit-nota,dt-confirma,nr-pedcli,cod-entrega,nome-transp,cidade-cif,cidade,estado,cep,cgc,vl-mercad,vl-frete,vl-seguro,vl-embalagem,nr-fatura,nat-operacao,emite-duplic,cod-mensagem,nr-volumes,peso-liq-tot,peso-bru-tot,ind-tip-nota,dt-saida,cod-emitente,idi-sit-nf-eletro,cod-chave-aces-nf-eletro,idi-forma-emis-nf-eletro" .
   //item 
    //ped-venda ASSIGN cListaCampos = "nr-pedido,nr-pedcli,nome-abrev,cod-estabel,cod-emitente,vl-tot-ped,dt-emissao,dt-implant,dt-entrega,dt-cancela,nat-operacao,cod-priori,cod-entrega,cidade-cif,cidade,estado,cep,cgc,vl-liq-ped,vl-liq-abe,no-ab-reppri,cod-sit-aval,cod-sit-ped,dsp-pre-fat,cod-sit-com,val-pct-desconto-total,des-pct-desconto-inform,cod-sit-preco".
    IF tt.tabela = 'nota-fiscal' THEN
        ASSIGN cListaCampos = "cod-estabel,serie,nr-nota-fis,nome-ab-cli,dt-emis-nota,ind-sit-nota,dt-confirma,hr-confirma,dt-cancela,cod-cond-pag,nr-pedcli,cod-entrega,bairro,cidade,estado,cep,cgc,ins-estadual,cidade-cif,nome-transp,tp-preco,ind-lib-nota,nr-tabpre,vl-tot-nota,vl-mercad,vl-frete,vl-seguro,vl-embalagem,nr-fatura,nat-operacao,emite-duplic,cod-mensagem,preco-saida,nr-volumes,peso-liq-tot,peso-bru-tot,ind-tip-nota,cod-portador,modalidade,dt-prvenc,no-ab-reppri,cod-rep,nome-ab-reg,cod-emitente,vl-fatura,vl-comis-nota,vl-tot-com,vl-tot-ipi,vl-merc-tot-fat,cod-canal-venda,vl-tot-nota-me,vl-mercad-me,vl-frete-me,vl-seguro-me,vl-embalagem-me,vl-acum-dup-me,vl-fatura-me,vl-tot-itens-fat-me,vl-tot-com-me,vl-comis-nota-me,nome-abrev-tri,idi-sit-nf-eletro,cod-chave-aces-nf-eletro,idi-forma-emis-nf-eletro".
    IF tt.tabela = 'natur-oper' THEN
        ASSIGN cListaCampos = 'nat-operacao,tipo,denominacao,cd-trib-icm,aliquota-icm,perc-red-icm,cd-trib-ipi,cd-vinc-ipi,perc-red-ipi,cd-trib-iss,perc-red-iss,emite-duplic,cod-mensagem,atual-estat,mercado,preco-saida,manut-icm,manut-ipi,consum-final,baixa-estoq,narrativa,tp-rec-desp'.
    IF tt.tabela = 'ped-item' THEN
        ASSIGN cListaCampos = 'nome-abrev,nr-pedcli,nr-sequencia,it-codigo,nr-ordem,parcela,dt-entorig,dt-entrega,dt-canseq,desc-cancela,dt-reativ,dt-suspensao,qt-pedida,qt-atendida,qt-pendente,qt-devolvida,dt-devolucao,desc-devol,vl-pretab,vl-preori,vl-preuni,per-des-item,per-minfat,cod-sit-item,user-impl,dt-userimp,user-alte,dt-useralt,user-canc,dt-usercan,user-reat,dt-userrea,user-devol,dt-userdev,vl-merc-abe,vl-liq-it,vl-liq-abe,vl-tot-it,tp-preco,nat-operacao,tipo-atend,qt-fatenf,cod-refer,qt-alocada,cod-sit-pre,dt-min-fat,esp-ped,cod-sit-com,val-desconto-inform,log-usa-tabela-desconto,des-pct-desconto-inform,val-pct-desconto-tab-preco,val-pct-desconto-total,val-desconto-total,qt-un-fat,vl-preori-un-fat,user-aprov-cot,dt-aprov-cot,user-lib-cot,dt-lib-cot,cod-sit-preco,val-frete,val-seguro,val-embal,cod-mot-canc-cot,dat-aprov-preco,des-un-medida,ind-orig-entrada,cod-ord-compra'.
    IF tt.tabela = 'item' THEN
       ASSIGN cListaCampos = 'it-codigo,descricao-1,descricao-2,narrativa,desc-item,un,ge-codigo,fm-codigo,data-implant,deposito-pad,peso-liquido,peso-bruto,fm-cod-com,cod-obsoleto,quant-segur,lote-multipl,class-fiscal,preco-base,preco-ul-ent,preco-repos,vl-mat-ant,vl-mob-ant'.
    RUN limparTTs IN hBoConsDin.
    RUN incluirCpsTabela      IN hBoConsDin(INPUT TABLE ttCampos).  
    RUN setArqInsertSqlite    IN hBoConsDin('c:\temp\importante\' + tt.tabela + '.sql').
    RUN setBancoTabela IN hBoConsDin('cliente','ems5').
    RUN setBancoTabela IN hBoConsDin('fornecedor','ems5').
    RUN setDadosConsulta IN hBoConsDin(tt.tabela,cListaCampos,'1=1','').
    RUN setCaracterSubst IN hBoConsDin('-','_').
    RUN execConsulta IN hBoConsDin('postgresql').

END.
 
RUN finalizarBos IN hBoConsDin.
IF VALID-HANDLE(hBoconsDin) THEN
   DELETE PROCEDURE hBoConsDin.
IF VALID-HANDLE(hBoMD) THEN
   DELETE PROCEDURE hboMd.



/*UPDATE cTabela.

//RUN setBanco IN hBoMD(cBancoSel).




OUTPUT TO c:\temp\ttCampos.txt.
FOR EACH ttCampos:
    DISP ttCampos.nome .
END.
OUTPUT CLOSE.

*/

