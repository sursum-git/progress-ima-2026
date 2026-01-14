DEF VAR r-nota AS ROWID.
DEF VAR c-desc-mod-frete AS CHAR.

DEFINE VARIABLE axsep017                   AS HANDLE      NO-UNDO.

{ftp/ft0518f.i5} /* ttDanfe, ttDanfeItem */

{adapters/xml/ep2/axsep017.i}

FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '5' AND
     nota-fiscal.serie = '3' AND
     nota-fiscal.nr-nota-fis = '0193546'
     NO-LOCK NO-ERROR.
     
ASSIGN r-nota = ROWID(nota-fiscal).

RUN adapters/xml/ep2/axsep017.p PERSISTENT SET axsep017.
RUN pi-seta-nota-fiscal    IN axsep017 (INPUT r-nota).
RUN pi-prepara-dados       IN axsep017.
RUN pi-devolve-temp-tables IN axsep017 (OUTPUT  TABLE ttAdi       ,
                                          OUTPUT  TABLE ttArma      ,
                                          OUTPUT  TABLE ttAvulsa    ,
                                          OUTPUT  TABLE ttCobr      ,
                                          OUTPUT  TABLE ttCOFINSAliq,
                                          OUTPUT  TABLE ttCOFINSNT  ,
                                          OUTPUT  TABLE ttCOFINSOutr,
                                          OUTPUT  TABLE ttCOFINSQtde,
                                          OUTPUT  TABLE ttCOFINSST  ,
                                          OUTPUT  TABLE ttComb      ,
                                          OUTPUT  TABLE ttCompra    ,
                                          OUTPUT  TABLE ttDest      ,
                                          OUTPUT  TABLE ttDet       ,
                                          OUTPUT  TABLE ttDI        ,
                                          OUTPUT  TABLE ttDup       ,
                                          OUTPUT  TABLE ttEmit      ,
                                          OUTPUT  TABLE ttEntrega   ,
                                          OUTPUT  TABLE ttExporta   ,
                                          OUTPUT  TABLE ttICMS00    ,
                                          OUTPUT  TABLE ttICMS10    ,
                                          OUTPUT  TABLE ttICMS20    ,
                                          OUTPUT  TABLE ttICMS30    ,
                                          OUTPUT  TABLE ttICMS40    ,
                                          OUTPUT  TABLE ttICMS51    ,
                                          OUTPUT  TABLE ttICMS60    ,
                                          OUTPUT  TABLE ttICMS70    ,
                                          OUTPUT  TABLE ttICMS90    ,
                                          OUTPUT  TABLE ttICMSTot   ,
                                          OUTPUT  TABLE ttIde       ,
                                          OUTPUT  TABLE ttII        ,
                                          OUTPUT  TABLE ttInfAdic   ,
                                          OUTPUT  TABLE ttIPI       ,
                                          OUTPUT  TABLE ttISSQN     ,
                                          OUTPUT  TABLE ttISSQNtot  ,
                                          OUTPUT  TABLE ttLacres    ,
                                          OUTPUT  TABLE ttMed       ,
                                          OUTPUT  TABLE ttNFe       ,
                                          OUTPUT  TABLE ttrefNF     ,
                                          OUTPUT  TABLE ttObsCont   ,
                                          OUTPUT  TABLE ttObsFisco  ,
                                          OUTPUT  TABLE ttPISAliq   ,
                                          OUTPUT  TABLE ttPISNT     ,
                                          OUTPUT  TABLE ttPISOutr   ,
                                          OUTPUT  TABLE ttPISQtde   ,
                                          OUTPUT  TABLE ttPISST     ,
                                          OUTPUT  TABLE ttProcRef   ,
                                          OUTPUT  TABLE ttReboque   ,
                                          OUTPUT  TABLE ttRetirada  ,
                                          OUTPUT  TABLE ttRetTrib   ,
                                          OUTPUT  TABLE ttTransp    ,
                                          OUTPUT  TABLE ttVeic      ,
                                          OUTPUT  TABLE ttVol       ,
                                          OUTPUT  TABLE ttrefNFP    ,
                                          OUTPUT  TABLE ttrefCTe    ,
                                          OUTPUT  TABLE ttrefECF    ,
                                          OUTPUT  TABLE ttICMSPart  ,
                                          OUTPUT  TABLE ttICMSST    ,
                                          OUTPUT  TABLE ttICMSSN101 ,
                                          OUTPUT  TABLE ttICMSSN102 ,
                                          OUTPUT  TABLE ttICMSSN201 ,
                                          OUTPUT  TABLE ttICMSSN202 ,
                                          OUTPUT  TABLE ttICMSSN500 ,
                                          OUTPUT  TABLE ttICMSSN900 ,
                                          OUTPUT  TABLE ttCana      ,
                                          OUTPUT  TABLE ttForDia    ,
                                          OUTPUT  TABLE ttDeduc     ).
                                          
FIND FIRST ttTransp NO-ERROR.

MESSAGE ttTransp.modfrete
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

CASE ttTransp.modfrete:
    WHEN "0":U THEN
        ASSIGN c-desc-mod-frete = "0 Î Emitente":U.
    WHEN "1":U THEN DO.
        IF nota-fiscal.nome-tr-red <> "" THEN
           ASSIGN c-desc-mod-frete = "0 Î Emitente/Destinatario":U. 
        ELSE
           ASSIGN c-desc-mod-frete = "1 Î Destinatario":U. 
    END.
    WHEN "2":U THEN
        ASSIGN c-desc-mod-frete = "2 Î Terceiros":U.
    WHEN "9":U THEN
        ASSIGN c-desc-mod-frete = "9 Î Sem Frete":U.
END CASE.

MESSAGE  c-desc-mod-frete
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
 
