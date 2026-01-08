def VAR c-dbase as char.

disable triggers for load of ems2med.emitente.
disable triggers for load of ems2med.transporte.
disable triggers for load of ems2med.repres.
disable triggers for load of ems2med.estabelec.
disable triggers for load of ems2med.param-nf-estab.

    assign c-dbase = "MED TESTE".


for each ems2med.estabelec share-lock where
         ems2med.estabelec.cod-estabel = "1" or
         ems2med.estabelec.cod-estabel = "5".
    
/* cd0403 */     
    assign ems2med.estabelec.nome                   = c-dbase + " " + ems2med.estabelec.nome
           ems2med.estabelec.des-local-arq          = replace(ems2med.estabelec.des-local-arq,"PROD","HOMO")
           ems2med.estabelec.idi-tip-emis-nf-eletro = 2.

    find first ems2med.param-nf-estab where
               ems2med.param-nf-estab.cod-estabel = ems2med.estabelec.cod-estabel
               no-error.
	if avail(ems2med.param-nf-estab) then do:
       assign ems2med.param-nf-estab.idi-tip-emis-amb-sefaz = 1.
       overlay(ems2med.param-nf-estab.cod-livre-1,216,1)    = "2".
       overlay(ems2med.param-nf-estab.cod-livre-1,229,1)    = "2".
	end.
end.

/*
for each ems2med.emitente share-lock:
  assign ems2med.emitente.e-mail    = c-email
         ems2med.emitente.nome-emit = c-dbase + " " + ems2med.emitente.nome-emit.
end.

for each ems2med.transporte share-lock:
  assign ems2med.transporte.e-mail = c-email
         ems2med.transporte.nome   = c-dbase + " " + ems2med.transporte.nome.
end.

for each ems2med.repres share-lock:
  assign ems2med.repres.e-mail = c-email
         ems2med.repres.nome   = c-dbase + " " + ems2med.repres.nome.
end.
*/
