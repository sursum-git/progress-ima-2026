/****************************************************************************
programa:esapi/getDevolVenda.p
objetivo: retornar uma tabela temporaria com os dados das notas fiscais
de devolu‡Æo conforme parametros.
data: 11/2025
autor: tadeu silva
***************************************************************************/
{esapi/getDevolVenda.i}

DEFINE INPUT  PARAMETER pEstab      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDtEmisIni  AS DATE        NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttResult.


FOR EACH docum-est FIELDS(cod-estabel serie-docto nro-docto cod-emitente nat-operacao tot-valor dt-emissao dt-trans) NO-LOCK
    WHERE docum-est.cod-estabel = pEstab
    AND   docum-est.dt-emissao >= pDtEmisIni
    AND NOT CAN-FIND( FIRST etq_devol 
    WHERE etq_devol.cod_estabel     = docum-est.cod-estabel
    AND   etq_devol.serie           = docum-est.serie-docto
    AND   etq_devol.nr_docto        = docum-est.nro-docto
    AND   etq_devol.nat_operacao    = docum-est.nat-operacao  
    )
    USE-INDEX estab-dtemissao,
    EACH emitente FIELDS(cod-emitente nome-abrev nome-emit cgc) OF  docum-est NO-LOCK,
    EACH natur-oper fields(nat-operacao denominacao ) OF docum-est
    WHERE natur-oper.tipo-compra = 3 NO-LOCK:
    CREATE ttResult.
    ASSIGN ttResult.estab           = docum-est.cod-estabel
           ttResult.serie           = docum-est.serie
           ttResult.documento       = docum-est.nro-docto
           ttResult.cliente         = STRING(docum-est.cod-emitente) + "-" + emitente.nome-emit + "(" + emitente.cgc + ")"
           ttResult.natureza        = docum-est.nat-operacao + "-" + natur-oper.denominacao
           ttResult.dt_emissao      = docum-est.dt-emissao
           ttResult.dt_transacao    = docum-est.dt-trans
           ttResult.vl_nota         = docum-est.tot-valor
          .    
END.

CATCH eSysError AS Progress.Lang.SysError:
    UNDO, THROW NEW Progress.Lang.AppError(eSysError:GetMessage(1), eSysError:GetMessageNum(1)).          
    
END CATCH.

CATCH eAppError AS Progress.Lang.AppError:
    UNDO, THROW NEW Progress.Lang.AppError(eAppError:GetMessage(1), eAppError:GetMessageNum(1)).          
END CATCH.

/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 etq_devol_id                     int6        i
   20 cod_estabel                      char        i
   30 serie                            char        i
   40 nr_docto                         char        i
   50 nat_operacao                     char        i
   60 it_codigo                        char        i
   70 cod_refer                        char        i
   80 num_sequencia                    inte        i
   90 num_etiqueta                     inte
*/
