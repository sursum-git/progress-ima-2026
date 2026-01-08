def VAR c-base  as char.
def VAR c-dbase as char.

disable triggers for load of ems2ima.emitente.
disable triggers for load of ems2ima.transporte.
disable triggers for load of ems2ima.repres.
disable triggers for load of ems2ima.estabelec.
disable triggers for load of ems2ima.param-nf-estab.

assign c-dbase = "IMA TESTE".

for each ems2ima.estabelec share-lock where
         ems2ima.estabelec.cod-estabel = "1" or
         ems2ima.estabelec.cod-estabel = "5".
    
/* cd0403 */     
    assign ems2ima.estabelec.nome                   = c-dbase + " " + ems2ima.estabelec.nome
           ems2ima.estabelec.des-local-arq          = replace(ems2ima.estabelec.des-local-arq,"PROD","HOMO")
           ems2ima.estabelec.idi-tip-emis-nf-eletro = 2.

    find first ems2ima.param-nf-estab where
               ems2ima.param-nf-estab.cod-estabel = ems2ima.estabelec.cod-estabel
               no-error.
	if avail(ems2ima.param-nf-estab) then do:
       assign ems2ima.param-nf-estab.idi-tip-emis-amb-sefaz = 1.
       overlay(ems2ima.param-nf-estab.cod-livre-1,216,1)    = "2".
       overlay(ems2ima.param-nf-estab.cod-livre-1,229,1)    = "2".
	end.
end.

/*
for each ems2ima.emitente share-lock: 
  assign ems2ima.emitente.e-mail    = c-email
         ems2ima.emitente.nome-emit = c-dbase + " " + ems2ima.emitente.nome-emit.
end.

for each ems2ima.transporte share-lock:
  assign ems2ima.transporte.e-mail = c-email
         ems2ima.transporte.nome   = c-dbase + " " + ems2ima.transporte.nome.
end.

for each ems2ima.repres share-lock:
  assign ems2ima.repres.e-mail = c-email
         ems2ima.repres.nome   = c-dbase + " " + ems2ima.repres.nome.
end.
*/
