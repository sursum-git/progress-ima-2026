{cdp/cd0666.i}
{cep/ceapi001.i}
{utp/ut-glob.i}
{esp/util.i}
{esapi/getSaldoEstoqPorEstabDtDepos.i}

//Item;Est;Dep;Localizacao;Lote;Refer.;Validade;Dt Fabrica‡Æo;Un;Qtd Liquida
 DEFINE VARIABLE iCont      AS INTEGER     NO-UNDO.
 DEFINE VARIABLE qtAcresc   AS DECIMAL     NO-UNDO.
 DEFINE TEMP-TABLE ttSaldo NO-UNDO
    FIELD produto       LIKE ITEM.it-codigo
    FIELD refer         AS CHAR
    FIELD quantidade    AS DECIMAL    
    FIELD qtFinal       AS DECIMAL
    FIELD nrDocto       AS CHAR
    FIELD lancGerado    AS LOGICAL
    .
    
 INPUT FROM VALUE("p:\inventario 2025\base-etq-lisa-para-td.csv").
       REPEAT:
       CREATE ttSaldo.
       IMPORT DELIMITER ";" ttSaldo.       
       END. 
 INPUT CLOSE.
 
 RUN esapi/getSaldoEstoqPorEstabDtDepos.p('505','arm',12.22.2025,OUTPUT TABLE ttQT).
 //{esp/exportarTabelaCsv3.i ttsaldo " " " " "ttsaldo-antes"} 
 {esp/exportarTabelaCsv3.i ttQt " " " " "ttqt"} 
 FIND FIRST param-estoq NO-LOCK NO-ERROR.
 OUTPUT TO VALUE('c:\temp\log_composicao_saldo_arm.csv').
 
 FOR EACH ttSaldo WHERE ttSaldo.refer <> '000':
    ASSIGN iCont = iCont + 1.
    ASSIGN ttSaldo.nrDocto = 'inv25/2-' + STRING(iCont).
    ASSIGN ttSaldo.refer = IF LENGTH(ttSaldo.refer) < 3 THEN FILL('0', 3 - LENGTH(ttSaldo.refer))  + ttSaldo.refer 
                           ELSE ttSaldo.refer. 
                           
     PUT UNFORM FILL("=" , 200 )SKIP.
     PUT UNFORM "Produto:" ttSaldo.produto  " Ref:"  ttSaldo.refer  " Quantidade:" ttSaldo.quantidade SKIP.
     
    //pegar o saldo da data de referencia e somar a quantidade da ttSaldo e criar a td com esta quantidade de entrada
     FIND ttQt
     WHERE ttQt.itCodigo    = ttSaldo.produto
     AND   ttQt.codRefer    = ttSaldo.refer
     NO-ERROR.
     IF AVAIL ttQt THEN DO:
        ASSIGN qtAcresc = ttQt.qtSaldoData.         
        PUT UNFORM  "qte.atual estoque:" ttQt.qtSaldoAtu " qt.entrada pos:" ttQt.qtEntradaPos   " qt.saida pos:" ttQT.qtSaidaPos " qt.saldo data:" ttQt.qtSaldoData
        SKIP .
     END.
     ASSIGN ttSaldo.qtFinal = ttsaldo.quantidade + qtAcresc - ttQt.qtsaldoAtu .
     PUT UNFORM "nova quantidade a gerar a td:"  ttSaldo.qtFinal  SKIP.
    
    FOR FIRST ITEM FIELDS(it-codigo un)
    WHERE ITEM.it-codigo = ttSaldo.produto:
    END.                                                                     
    //DISP ttSaldo.
    IF ttSaldo.qtFinal  <= 0 THEN DO:
       NEXT.
        
    END.
    
    
    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "inv25/2"                        /* acrescentado*/
           tt-movto.usuario                 = c-seg-usuario                         /* acrescentado*/
           tt-movto.cod-estabel             = '505'                            /* param-estoq.estabel-pad */
           tt-movto.ct-codigo               = param-estoq.ct-acerto
           tt-movto.sc-codigo               = param-estoq.sc-acerto
           tt-movto.conta-contabil          = param-estoq.conta-acerto
           tt-movto.esp-docto               = 6
           tt-movto.tipo-trans              = 1
           tt-movto.cod-depos               = 'ARM'
           tt-movto.dt-trans                = TODAY
           tt-movto.it-codigo               = ttSaldo.produto
           tt-movto.cod-refer               = ttSaldo.refer
           tt-movto.lote                    = ttSaldo.refer
           tt-movto.quantidade              = ttSaldo.qtFinal
           tt-movto.un                      = ITEM.un
           tt-movto.dt-vali-lote            = 12.31.9999           
           tt-movto.nro-docto               = ttsaldo.nrDocto
           .     
 END.
 OUTPUT CLOSE.
 FOR EACH tt-movto
 WHERE tt-movto.cod-refer = '000':
    DELETE tt-movto.
 END.
 {esp/exportarTabelaCsv3.i tt-movto " " " except r-mov-inv r-mov-orig" "movto-td"} 
RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                    INPUT-OUTPUT TABLE tt-erro,
                    INPUT YES). /* Deleta erros? */
 
 
FIND FIRST tt-erro
WHERE cd-erro <> 56847 NO-LOCK NO-ERROR.     
IF AVAIL tt-erro THEN DO.
   FOR EACH tt-erro.
       MESSAGE "Erro ao reportar o item " SKIP
               tt-erro.mensagem
               tt-erro.cd-erro
               VIEW-AS ALERT-BOX ERROR.
   END.
END. 
 
 
FOR EACH ttSaldo:
    ASSIGN ttSaldo.lancGerado = 
    CAN-FIND( FIRST movto-estoq
    WHERE movto-estoq.cod-estabel = '505'
    AND   movto-estoq.cod-depos = 'arm'
    AND   movto-estoq.it-codigo = ttSaldo.produto
    AND   movto-estoq.cod-refer = ttsaldo.refer
    AND   movto-estoq.nro-docto = ttSaldo.nrDocto
    AND   movto-estoq.dt-trans  =  TODAY)
    .   
END.
{esp/exportarTabelaCsv3.i ttSaldo " " " " "conf-td-saldo"} 

