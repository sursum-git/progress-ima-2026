DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.
DEFINE VARIABLE cArqExcel AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTipoAtividade AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescTipoAtiv  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codCnae        AS CHARACTER   NO-UNDO.

RUN esbo/bonat001.p PERSISTENT SET  hBo.

DEFINE TEMP-TABLE tt
    FIELD data          AS DATE
    FIELD nota          AS CHAR
    FIELD natOperacao   AS CHAR FORMAT 'x(20)'
    FIELD descNatOper   AS CHAR 
    FIELD codEmitente   AS INT
    FIELD nomeEmit      LIKE emitente.nome-emit
    FIELD tipoAtividade AS CHAR
    FIELD codCnae       AS CHAR
    FIELD codcnaeAtac   AS CHAR
    FIELD cfop          AS CHAR
    FIELD vlItem        AS DECIMAL
    FIELD vlLiqMerc     AS DECIMAL
    FIELD baseICMS      AS DECIMAL
    FIELD percICMS      AS DECIMAL
    FIELD vlICMS        AS DECIMAL
    FIELD vlPIS         AS decimal
    FIELD vlCOFINS      AS DECIMAL.

DEFINE VARIABLE cCNAEAtac AS CHARACTER   NO-UNDO.


FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= dtIni
    AND   nota-fiscal.dt-emis-nota <= dtFim
    AND  nota-fiscal.dt-cancel = ? 
    AND  nota-fiscal.estado = 'es'.
    FIND emitente OF nota-fiscal NO-LOCK NO-ERROR.
    RUN retornarTipoAtividadeCliente IN hBo(emitente.cod-emitente, OUTPUT iTipoAtividade, OUTPUT codCnae).
    RUN buscarDescTipoAtividade IN hBo(INPUT iTipoAtividade ,OUTPUT cDescTipoAtiv ).
    FOR EACH  it-nota-fisc OF nota-fiscal NO-LOCK .
       FIND natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
       CREATE tt.
       ASSIGN 
       tt.data              =  nota-fiscal.dt-emis-nota              
       tt.nota              =  nota-fiscal.nr-nota-fis
       tt.natOperacao       =  nota-fiscal.nat-operacao
       tt.descNatOper       =  natur-oper.denominacao
       tt.codEmitente       =  emitente.cod-emitente
       tt.nomeEmit          =  emitente.nome-emit
       tt.tipoAtividade     =  cDescTipoAtiv
       tt.codcnae           =  codCNAE.
       RUN getCNAEsecundAtacado(emitente.cod-emitente, tt.codCNAE, OUTPUT cCNAEAtac).
       ASSIGN 
       tt.codCnaeAtac       = IF tt.tipoAtividade = 'atacado' THEN tt.codCnae ELSE cCnaeAtac 
       tt.cfop              = natur-oper.cod-cfop
       tt.vlItem            = it-nota-fisc.vl-tot-item
       tt.percICMS          = it-nota-fisc.aliquota-icm
       tt.vlICMS            = it-nota-fisc.vl-icms-it
       tt.baseICMS          = it-nota-fisc.vl-bicms-it
       tt.vlLiqMerc         = it-nota-fisc.vl-merc-liq    
       tt.vlCofins          = it-nota-fisc.vl-finsocial
       tt.vlPis             = it-nota-fisc.vl-pis .  
       //EXPORT DELIMITER ";"   
    END.
END.
OUTPUT TO c:\temp\nfs_es.csv.
PUT "data;nota;nat.oper;descricao;cod.emitente;nome emit;tipo Ativ.;CNAE tipo Ativ.;CNAE ATACADO;CFOP;vlltot.item;vl.liq.merc;vl.base icms;aliq.icm;vl.icms;vl.pis;vl.cofins" SKIP.
FOR EACH tt:
    EXPORT DELIMITER ";" tt.

END.

OUTPUT CLOSE.

ASSIGN cArqExcel = SEARCH('excel/nfs_es.xlsx').
IF cArqExcel <> ? THEN
    OS-COMMAND SILENT VALUE('start excel /t ' + cArqExcel).
ELSE 
   MESSAGE "arquivo excel/nfs_es.xlsx n∆o encontrado "
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

PROCEDURE getCNAEsecundAtacado:
    DEFINE INPUT  PARAMETER iEmitente AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER cCNAE     AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cnaeAtac  AS CHARACTER   NO-UNDO.
    FOR EACH emitente_cnae NO-LOCK
        WHERE emitente_cnae.cod_emitente = iEmitente
        AND   emitente_cnae.cod_cnae     <>  cCNAE :
        FIND cnaes WHERE
            cnaes.cod_cnae = emitente_cnae.cod_cnae
            NO-LOCK NO-ERROR.
        IF AVAIL cnaes AND cnaes.ind_tipo_atividade = '2' THEN DO:
           ASSIGN cnaeAtac = cnaes.cod_cnae.
           LEAVE.
        END.     
    END.
END PROCEDURE.
