/*
**
**	Created by PROGRESS ProxyGen (Versão do Progress 11.7) Tue May 24 20:21:32 BRT 2022
**
*/

//
// boad098Af
//



    using System;
    using Progress.Open4GL.DynamicAPI;
    using Progress.Open4GL.Proxy;
    using Progress.Open4GL;
    using Progress.Open4GL.Exceptions;

    public class boad098Af : Procedure
    {
        // Create a DataTableMetaData object for each temp-table or
        // datatable parm used in any and all methods. Create a
        // DataTableSchema object for each method call that has
        // temp-table or datatable parms which points to one or more
        // temp-tables or datatables used in that method call.


	static DataTableMetaData getRowErrors_MetaData1;



	static DataTableMetaData selfInfo_MetaData1;



	static DataTableMetaData getBatchRawRecords_MetaData1;



	static DataTableMetaData getBatchRawRecordsPrev_MetaData1;



	static DataTableMetaData getBatchRecords_MetaData1;



	static DataTableMetaData getBatchRecordsPrev_MetaData1;



	static DataTableMetaData getRecord_MetaData1;



	static DataTableMetaData setRecord_MetaData1;



	static DataTableMetaData findRowidShow_MetaData1;



	static DataTableMetaData getCurrent_MetaData1;



	static DataTableMetaData serverSendRows_MetaData1;



	static DataTableMetaData validateCreate_MetaData1;

	static DataTableMetaData validateCreate_MetaData2;



	static DataTableMetaData validateDelete_MetaData1;



	static DataTableMetaData validateUpdate_MetaData1;

	static DataTableMetaData validateUpdate_MetaData2;




        static boad098Af()
        {
		getRowErrors_MetaData1 = new DataTableMetaData(0, "RowErrors", 7, false, 0, null, null, null, "StrongTypesNS.RowErrorsDataTable");
		getRowErrors_MetaData1.setFieldDesc(1, "ErrorSequence", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		getRowErrors_MetaData1.setFieldDesc(2, "ErrorNumber", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		getRowErrors_MetaData1.setFieldDesc(3, "ErrorDescription", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		getRowErrors_MetaData1.setFieldDesc(4, "ErrorParameters", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		getRowErrors_MetaData1.setFieldDesc(5, "ErrorType", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		getRowErrors_MetaData1.setFieldDesc(6, "ErrorHelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		getRowErrors_MetaData1.setFieldDesc(7, "ErrorSubType", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		selfInfo_MetaData1 = new DataTableMetaData(0, "RowInfo", 2, false, 0, null, null, null, "StrongTypesNS.RowInfoDataTable");
		selfInfo_MetaData1.setFieldDesc(1, "Code", 0, Parameter.PRO_CHARACTER, 0, 0, 0, "", "", 0, null, "");
		selfInfo_MetaData1.setFieldDesc(2, "CodeValue", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");

		getBatchRawRecords_MetaData1 = new DataTableMetaData(0, "RowRaw", 1, false, 0, null, null, null, "StrongTypesNS.RowRawDataTable");
		getBatchRawRecords_MetaData1.setFieldDesc(1, "RawRecord", 0, Parameter.PRO_RAW, 0, 0, 0, "", "", 0, null, "");

		getBatchRawRecordsPrev_MetaData1 = new DataTableMetaData(0, "RowRaw", 1, false, 0, null, null, null, "StrongTypesNS.RowRawDataTable");
		getBatchRawRecordsPrev_MetaData1.setFieldDesc(1, "RawRecord", 0, Parameter.PRO_RAW, 0, 0, 0, "", "", 0, null, "");

		getBatchRecords_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		getBatchRecords_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		getBatchRecordsPrev_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		getBatchRecordsPrev_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		getRecord_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		getRecord_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		setRecord_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		setRecord_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		findRowidShow_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		findRowidShow_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		getCurrent_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		getCurrent_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		serverSendRows_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 200, false, 2, "1,cod-emitente:codigo.nome-abrev:nome", null, null, "StrongTypesNS.RowObjectAuxDataTable");
		serverSendRows_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		validateCreate_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectAuxDataTable");
		validateCreate_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");
		validateCreate_MetaData2 = new DataTableMetaData(0, "RowErrors", 7, false, 0, null, null, null, "StrongTypesNS.RowErrorsDataTable");
		validateCreate_MetaData2.setFieldDesc(1, "ErrorSequence", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(2, "ErrorNumber", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(3, "ErrorDescription", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(4, "ErrorParameters", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(5, "ErrorType", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(6, "ErrorHelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(7, "ErrorSubType", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		validateDelete_MetaData1 = new DataTableMetaData(0, "RowErrors", 7, false, 0, null, null, null, "StrongTypesNS.RowErrorsDataTable");
		validateDelete_MetaData1.setFieldDesc(1, "ErrorSequence", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(2, "ErrorNumber", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(3, "ErrorDescription", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(4, "ErrorParameters", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(5, "ErrorType", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(6, "ErrorHelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(7, "ErrorSubType", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		validateUpdate_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 2, "1,cod-emitente:codigo.nome-abrev:nome", null, null, "StrongTypesNS.RowObjectAuxDataTable");
		validateUpdate_MetaData1.setFieldDesc(1, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(2, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 1, 4096, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(3, "cgc", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(4, "identific", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(5, "natureza", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(6, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(7, "endereco", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(8, "bairro", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(9, "cidade", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(10, "estado", 0, Parameter.PRO_CHARACTER, 0, 9, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(11, "cep", 0, Parameter.PRO_CHARACTER, 0, 10, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(12, "caixa-postal", 0, Parameter.PRO_CHARACTER, 0, 11, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(13, "pais", 0, Parameter.PRO_CHARACTER, 0, 12, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(14, "ins-estadual", 0, Parameter.PRO_CHARACTER, 0, 13, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(15, "cod-cond-pag", 0, Parameter.PRO_INTEGER, 0, 14, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(16, "taxa-financ", 0, Parameter.PRO_DECIMAL, 0, 15, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(17, "data-taxa", 0, Parameter.PRO_DATE, 0, 16, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(18, "cod-transp", 0, Parameter.PRO_INTEGER, 0, 17, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(19, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 18, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(20, "linha-produt", 0, Parameter.PRO_CHARACTER, 0, 19, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(21, "atividade", 0, Parameter.PRO_CHARACTER, 0, 20, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(22, "contato", 2, Parameter.PRO_CHARACTER, 0, 21, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(23, "telefone", 2, Parameter.PRO_CHARACTER, 0, 22, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(24, "ramal", 2, Parameter.PRO_CHARACTER, 0, 23, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(25, "telefax", 0, Parameter.PRO_CHARACTER, 0, 24, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(26, "ramal-fax", 0, Parameter.PRO_CHARACTER, 0, 25, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(27, "telex", 0, Parameter.PRO_CHARACTER, 0, 26, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(28, "data-implant", 0, Parameter.PRO_DATE, 0, 27, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(29, "compr-period", 0, Parameter.PRO_DECIMAL, 0, 28, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(30, "end-cobranca", 0, Parameter.PRO_INTEGER, 0, 29, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(31, "cod-rep", 0, Parameter.PRO_INTEGER, 0, 30, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(32, "categoria", 0, Parameter.PRO_CHARACTER, 0, 31, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(33, "bonificacao", 0, Parameter.PRO_DECIMAL, 0, 32, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(34, "istr", 0, Parameter.PRO_INTEGER, 0, 33, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(35, "cod-gr-cli", 0, Parameter.PRO_INTEGER, 0, 34, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(36, "lim-credito", 0, Parameter.PRO_DECIMAL, 0, 35, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(37, "dt-lim-cred", 0, Parameter.PRO_DATE, 0, 36, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(38, "perc-fat-ped", 0, Parameter.PRO_INTEGER, 0, 37, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(39, "portador", 0, Parameter.PRO_INTEGER, 0, 38, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(40, "modalidade", 0, Parameter.PRO_INTEGER, 0, 39, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(41, "ind-fat-par", 0, Parameter.PRO_LOGICAL, 0, 40, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(42, "ind-cre-cli", 0, Parameter.PRO_INTEGER, 0, 41, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(43, "ind-apr-cred", 0, Parameter.PRO_LOGICAL, 0, 42, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(44, "nat-operacao", 0, Parameter.PRO_CHARACTER, 0, 43, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(45, "observacoes", 0, Parameter.PRO_CHARACTER, 0, 44, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(46, "per-minfat", 0, Parameter.PRO_DECIMAL, 0, 45, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(47, "emissao-ped", 0, Parameter.PRO_INTEGER, 0, 46, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(48, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 47, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(49, "telef-modem", 0, Parameter.PRO_CHARACTER, 0, 48, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(50, "ramal-modem", 0, Parameter.PRO_CHARACTER, 0, 49, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(51, "telef-fac", 0, Parameter.PRO_CHARACTER, 0, 50, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(52, "ramal-fac", 0, Parameter.PRO_CHARACTER, 0, 51, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(53, "agencia", 0, Parameter.PRO_CHARACTER, 0, 52, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(54, "nr-titulo", 0, Parameter.PRO_INTEGER, 0, 53, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(55, "nr-dias", 0, Parameter.PRO_INTEGER, 0, 54, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(56, "per-max-canc", 0, Parameter.PRO_DECIMAL, 0, 55, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(57, "dt-ult-venda", 0, Parameter.PRO_DATE, 0, 56, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(58, "emite-bloq", 0, Parameter.PRO_LOGICAL, 0, 57, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(59, "emite-etiq", 0, Parameter.PRO_LOGICAL, 0, 58, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(60, "tr-ar-valor", 0, Parameter.PRO_INTEGER, 0, 59, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(61, "gera-ad", 0, Parameter.PRO_LOGICAL, 0, 60, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(62, "port-prefer", 0, Parameter.PRO_INTEGER, 0, 61, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(63, "mod-prefer", 0, Parameter.PRO_INTEGER, 0, 62, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(64, "bx-acatada", 0, Parameter.PRO_INTEGER, 0, 63, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(65, "conta-corren", 0, Parameter.PRO_CHARACTER, 0, 64, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(66, "nr-copias-ped", 0, Parameter.PRO_INTEGER, 0, 65, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(67, "cod-suframa", 0, Parameter.PRO_CHARACTER, 0, 66, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(68, "cod-cacex", 0, Parameter.PRO_CHARACTER, 0, 67, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(69, "gera-difer", 0, Parameter.PRO_INTEGER, 0, 68, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(70, "nr-tabpre", 0, Parameter.PRO_CHARACTER, 0, 69, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(71, "ind-aval", 0, Parameter.PRO_INTEGER, 0, 70, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(72, "user-libcre", 0, Parameter.PRO_CHARACTER, 0, 71, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(73, "ins-banc", 2, Parameter.PRO_INTEGER, 0, 72, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(74, "ven-feriado", 0, Parameter.PRO_INTEGER, 0, 73, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(75, "ven-domingo", 0, Parameter.PRO_INTEGER, 0, 74, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(76, "ven-sabado", 0, Parameter.PRO_INTEGER, 0, 75, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(77, "cgc-cob", 0, Parameter.PRO_CHARACTER, 0, 76, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(78, "cep-cob", 0, Parameter.PRO_CHARACTER, 0, 77, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(79, "estado-cob", 0, Parameter.PRO_CHARACTER, 0, 78, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(80, "cidade-cob", 0, Parameter.PRO_CHARACTER, 0, 79, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(81, "bairro-cob", 0, Parameter.PRO_CHARACTER, 0, 80, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(82, "endereco-cob", 0, Parameter.PRO_CHARACTER, 0, 81, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(83, "cx-post-cob", 0, Parameter.PRO_CHARACTER, 0, 82, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(84, "ins-est-cob", 0, Parameter.PRO_CHARACTER, 0, 83, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(85, "nome-mic-reg", 0, Parameter.PRO_CHARACTER, 0, 84, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(86, "tip-cob-desp", 0, Parameter.PRO_INTEGER, 0, 85, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(87, "nome-tr-red", 0, Parameter.PRO_CHARACTER, 0, 86, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(88, "nat-ope-ext", 0, Parameter.PRO_CHARACTER, 0, 87, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(89, "cod-banco", 0, Parameter.PRO_INTEGER, 0, 88, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(90, "prox-ad", 0, Parameter.PRO_INTEGER, 0, 89, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(91, "lim-adicional", 0, Parameter.PRO_DECIMAL, 0, 90, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(92, "dt-fim-cred", 0, Parameter.PRO_DATE, 0, 91, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(93, "obs-entrega", 0, Parameter.PRO_CHARACTER, 0, 92, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(94, "cod-tip-ent", 0, Parameter.PRO_INTEGER, 0, 93, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(95, "ins-municipal", 0, Parameter.PRO_CHARACTER, 0, 94, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(96, "nr-peratr", 0, Parameter.PRO_INTEGER, 0, 95, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(97, "nr-mesina", 0, Parameter.PRO_INTEGER, 0, 96, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(98, "insc-subs-trib", 0, Parameter.PRO_CHARACTER, 0, 97, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(99, "cod-mensagem", 0, Parameter.PRO_INTEGER, 0, 98, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(100, "nr-dias-taxa", 0, Parameter.PRO_INTEGER, 0, 99, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(101, "tp-desp-padrao", 0, Parameter.PRO_INTEGER, 0, 100, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(102, "tp-rec-padrao", 0, Parameter.PRO_INTEGER, 0, 101, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(103, "inf-complementar", 0, Parameter.PRO_CHARACTER, 0, 102, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(104, "zip-code", 0, Parameter.PRO_CHARACTER, 0, 103, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(105, "tp-inspecao", 0, Parameter.PRO_INTEGER, 0, 104, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(106, "forn-exp", 0, Parameter.PRO_LOGICAL, 0, 105, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(107, "tp-qt-prg", 0, Parameter.PRO_INTEGER, 0, 106, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(108, "ind-atraso", 0, Parameter.PRO_INTEGER, 0, 107, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(109, "ind-div-atraso", 0, Parameter.PRO_INTEGER, 0, 108, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(110, "ind-dif-atrs-1", 0, Parameter.PRO_INTEGER, 0, 109, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(111, "ind-dif-atrs-2", 0, Parameter.PRO_INTEGER, 0, 110, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(112, "esp-pd-venda", 0, Parameter.PRO_INTEGER, 0, 111, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(113, "ind-lib-estoque", 0, Parameter.PRO_LOGICAL, 0, 112, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(114, "ind-moeda-tit", 0, Parameter.PRO_INTEGER, 0, 113, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(115, "ind-rendiment", 0, Parameter.PRO_LOGICAL, 0, 114, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(116, "tp-pagto", 0, Parameter.PRO_INTEGER, 0, 115, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(117, "vl-min-ad", 0, Parameter.PRO_DECIMAL, 0, 116, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(118, "zip-cob-code", 0, Parameter.PRO_CHARACTER, 0, 117, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(119, "hora-ini", 0, Parameter.PRO_INTEGER, 0, 118, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(120, "hora-fim", 0, Parameter.PRO_INTEGER, 0, 119, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(121, "pais-cob", 0, Parameter.PRO_CHARACTER, 0, 120, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(122, "resumo-mp", 0, Parameter.PRO_INTEGER, 0, 121, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(123, "ind-cred-abat", 0, Parameter.PRO_LOGICAL, 0, 122, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(124, "contrib-icms", 0, Parameter.PRO_LOGICAL, 0, 123, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(125, "e-mail", 0, Parameter.PRO_CHARACTER, 0, 124, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(126, "home-page", 0, Parameter.PRO_CHARACTER, 0, 125, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(127, "ind-licenciador", 0, Parameter.PRO_LOGICAL, 0, 126, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(128, "endereco2", 0, Parameter.PRO_CHARACTER, 0, 127, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(129, "ind-aval-embarque", 0, Parameter.PRO_INTEGER, 0, 128, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(130, "ind-abrange-aval", 0, Parameter.PRO_INTEGER, 0, 129, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(131, "cod-tax", 0, Parameter.PRO_INTEGER, 0, 130, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(132, "cn-codigo", 0, Parameter.PRO_CHARACTER, 0, 131, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(133, "cod-entrega", 0, Parameter.PRO_CHARACTER, 0, 132, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(134, "cod-isencao", 0, Parameter.PRO_INTEGER, 0, 133, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(135, "dias-comp", 0, Parameter.PRO_INTEGER, 0, 134, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(136, "estoque", 0, Parameter.PRO_INTEGER, 0, 135, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(137, "flag-pag", 0, Parameter.PRO_INTEGER, 0, 136, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(138, "item-cli", 0, Parameter.PRO_LOGICAL, 0, 137, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(139, "moeda-libcre", 0, Parameter.PRO_INTEGER, 0, 138, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(140, "nr-dias-atraso", 0, Parameter.PRO_INTEGER, 0, 139, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(141, "valor-minimo", 0, Parameter.PRO_DECIMAL, 0, 140, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(142, "cod-parceiro-edi", 0, Parameter.PRO_INTEGER, 0, 141, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(143, "agente-retencao", 0, Parameter.PRO_LOGICAL, 0, 142, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(144, "percepcao", 0, Parameter.PRO_LOGICAL, 0, 143, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(145, "char-1", 0, Parameter.PRO_CHARACTER, 0, 144, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(146, "char-2", 0, Parameter.PRO_CHARACTER, 0, 145, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(147, "dec-1", 0, Parameter.PRO_DECIMAL, 0, 146, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(148, "dec-2", 0, Parameter.PRO_DECIMAL, 0, 147, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(149, "int-1", 0, Parameter.PRO_INTEGER, 0, 148, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(150, "int-2", 0, Parameter.PRO_INTEGER, 0, 149, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(151, "log-1", 0, Parameter.PRO_LOGICAL, 0, 150, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(152, "log-2", 0, Parameter.PRO_LOGICAL, 0, 151, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(153, "data-1", 0, Parameter.PRO_DATE, 0, 152, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(154, "data-2", 0, Parameter.PRO_DATE, 0, 153, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(155, "cod-canal-venda", 0, Parameter.PRO_INTEGER, 0, 154, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(156, "ind-sit-emitente", 0, Parameter.PRO_INTEGER, 0, 155, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(157, "ind-emit-retencao", 0, Parameter.PRO_INTEGER, 0, 156, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(158, "calcula-multa", 0, Parameter.PRO_LOGICAL, 0, 157, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(159, "prog-emit", 0, Parameter.PRO_LOGICAL, 0, 158, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(160, "nr-tab-progr", 0, Parameter.PRO_INTEGER, 0, 159, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(161, "recebe-inf-sci", 0, Parameter.PRO_LOGICAL, 0, 160, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(162, "cod-classif-fornec", 0, Parameter.PRO_INTEGER, 0, 161, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(163, "cod-classif-cliente", 0, Parameter.PRO_INTEGER, 0, 162, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(164, "nr-cheque-devol", 0, Parameter.PRO_INTEGER, 0, 163, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(165, "periodo-devol", 0, Parameter.PRO_INTEGER, 0, 164, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(166, "vl-max-devol", 0, Parameter.PRO_DECIMAL, 0, 165, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(167, "check-sum", 0, Parameter.PRO_CHARACTER, 0, 166, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(168, "val-quota-media", 0, Parameter.PRO_DECIMAL, 0, 167, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(169, "cod-repres-imp", 0, Parameter.PRO_INTEGER, 0, 168, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(170, "vencto-dia-nao-util", 0, Parameter.PRO_LOGICAL, 0, 169, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(171, "rend-tribut", 0, Parameter.PRO_DECIMAL, 0, 170, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(172, "utiliza-verba", 0, Parameter.PRO_LOGICAL, 0, 171, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(173, "percent-verba", 0, Parameter.PRO_DECIMAL, 0, 172, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(174, "endereco_text", 0, Parameter.PRO_CHARACTER, 0, 173, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(175, "endereco-cob-text", 0, Parameter.PRO_CHARACTER, 0, 174, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(176, "short-name", 0, Parameter.PRO_CHARACTER, 0, 175, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(177, "log-controla-val-max-inss", 0, Parameter.PRO_LOGICAL, 0, 176, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(178, "cod-inscr-inss", 0, Parameter.PRO_CHARACTER, 0, 177, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(179, "cod-pulmao", 0, Parameter.PRO_CHARACTER, 0, 178, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(180, "idi-tributac-cofins", 0, Parameter.PRO_INTEGER, 0, 179, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(181, "idi-tributac-pis", 0, Parameter.PRO_INTEGER, 0, 180, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(182, "log-calcula-pis-cofins-unid", 0, Parameter.PRO_LOGICAL, 0, 181, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(183, "log-optan-suspens-ipi", 0, Parameter.PRO_LOGICAL, 0, 182, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(184, "log-optan-cr-presmdo-subst", 0, Parameter.PRO_LOGICAL, 0, 183, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(185, "retem-pagto", 0, Parameter.PRO_LOGICAL, 0, 184, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(186, "portador-ap", 0, Parameter.PRO_INTEGER, 0, 185, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(187, "modalidade-ap", 0, Parameter.PRO_INTEGER, 0, 186, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(188, "log-contribt-subst-interm", 0, Parameter.PRO_LOGICAL, 0, 187, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(189, "dat-valid-suframa", 0, Parameter.PRO_DATE, 0, 188, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(190, "nom-fantasia", 0, Parameter.PRO_CHARACTER, 0, 189, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(191, "log-possui-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 190, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(192, "log-nf-eletro", 0, Parameter.PRO_LOGICAL, 0, 191, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(193, "dt-atualiza", 0, Parameter.PRO_DATE, 0, 192, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(194, "hra-atualiz", 0, Parameter.PRO_CHARACTER, 0, 193, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(195, "cod-email-nfe", 0, Parameter.PRO_CHARACTER, 0, 194, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(196, "log-integr-totvs-colab-vendas", 0, Parameter.PRO_LOGICAL, 0, 195, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(197, "cdn-atraso-max", 0, Parameter.PRO_INTEGER, 0, 196, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(198, "dat-nasc", 0, Parameter.PRO_DATE, 0, 197, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(199, "log-beneficiario", 0, Parameter.PRO_LOGICAL, 0, 198, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(200, "r-Rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");
		validateUpdate_MetaData2 = new DataTableMetaData(0, "RowErrors", 7, false, 0, null, null, null, "StrongTypesNS.RowErrorsDataTable");
		validateUpdate_MetaData2.setFieldDesc(1, "ErrorSequence", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(2, "ErrorNumber", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(3, "ErrorDescription", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(4, "ErrorParameters", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(5, "ErrorType", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(6, "ErrorHelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(7, "ErrorSubType", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");


        }

	//----Constructor

	public boad098Af(ProObject appObj) : base(appObj)
	{
                
		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Run procedure
		PersistProcObj = RunPersistentProcedure("boad098-af.p", parms);

		// Get output parameters



	}

        /// <summary>
	/// 
	/// </summary> 
	public int GetLastError()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		parms.setIntegerFunction();

		// Run procedure
		base.runProcedure("GetLastError", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		Object retObj = parms.FunctionReturnValue;
		if (retObj == null)
			throw new Open4GLException(BadOutputVal, null);
		return (int) retObj;

	}

/// <summary>
	/// 
	/// </summary> 
	public int GetParent(int hwnd)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, hwnd, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		parms.setIntegerFunction();

		// Run procedure
		base.runProcedure("GetParent", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		Object retObj = parms.FunctionReturnValue;
		if (retObj == null)
			throw new Open4GLException(BadOutputVal, null);
		return (int) retObj;

	}

/// <summary>
	/// 
	/// </summary> 
	public int ShowLastError()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		parms.setIntegerFunction();

		// Run procedure
		base.runProcedure("ShowLastError", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		Object retObj = parms.FunctionReturnValue;
		if (retObj == null)
			throw new Open4GLException(BadOutputVal, null);
		return (int) retObj;

	}

/// <summary>
	/// 
	/// </summary> 
	public int CreateProcess(string CommandLine, string CurrentDir, int wShowWindow)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setStringParameter(1, CommandLine, ParameterSet.INPUT);
		parms.setStringParameter(2, CurrentDir, ParameterSet.INPUT);
		parms.setIntegerParameter(3, wShowWindow, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		parms.setIntegerFunction();

		// Run procedure
		base.runProcedure("CreateProcess", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		Object retObj = parms.FunctionReturnValue;
		if (retObj == null)
			throw new Open4GLException(BadOutputVal, null);
		return (int) retObj;

	}

/// <summary>
	/// 
	/// </summary> 
	public string piSetaReturnValue(string ret)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, ret, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-seta-return-value", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _selfOthersInfo()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_selfOthersInfo", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _canRunMethod(string pFunction)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pFunction, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_canRunMethod", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string destroy()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("destroy", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string emptyRowErrors()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("emptyRowErrors", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string emptyRowInfo()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("emptyRowInfo", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getProtocol(out string pProtocol)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getProtocol", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pProtocol = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRowErrors(out StrongTypesNS.RowErrorsDataTable RowErrors)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowErrorsDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getRowErrors_MetaSchema = new MetaSchema();
		getRowErrors_MetaSchema.addDataTableSchema(getRowErrors_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getRowErrors", parms, getRowErrors_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowErrors = (StrongTypesNS.RowErrorsDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getTableName(out string pTableName)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getTableName", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pTableName = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string selfInfo(out StrongTypesNS.RowInfoDataTable RowInfo)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowInfoDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema selfInfo_MetaSchema = new MetaSchema();
		selfInfo_MetaSchema.addDataTableSchema(selfInfo_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("selfInfo", parms, selfInfo_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowInfo = (StrongTypesNS.RowInfoDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _insertError(int pErrorNumber, string pErrorType, string pErrorSubType, string pErrorParameters)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(4);

		// Set up input parameters
		parms.setIntegerParameter(1, pErrorNumber, ParameterSet.INPUT);
		parms.setStringParameter(2, pErrorType, ParameterSet.INPUT);
		parms.setStringParameter(3, pErrorSubType, ParameterSet.INPUT);
		parms.setStringParameter(4, pErrorParameters, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_insertError", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _insertErrorManual(int pErrorNumber, string pErrorType, string pErrorSubType, string pErrorDescription, string pErrorHelp, 
string pErrorParameters)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(6);

		// Set up input parameters
		parms.setIntegerParameter(1, pErrorNumber, ParameterSet.INPUT);
		parms.setStringParameter(2, pErrorType, ParameterSet.INPUT);
		parms.setStringParameter(3, pErrorSubType, ParameterSet.INPUT);
		parms.setStringParameter(4, pErrorDescription, ParameterSet.INPUT);
		parms.setStringParameter(5, pErrorHelp, ParameterSet.INPUT);
		parms.setStringParameter(6, pErrorParameters, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_insertErrorManual", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string bringCurrent()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("bringCurrent", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRawRecords(Rowid pRowid, bool pNext, int pRowsToReturn, out int pRowsReturned, out StrongTypesNS.RowRawDataTable RowRaw)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(5);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);
		parms.setBooleanParameter(2, pNext, ParameterSet.INPUT);
		parms.setIntegerParameter(3, pRowsToReturn, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(4, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(5, null, ParameterSet.OUTPUT, "StrongTypesNS.RowRawDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getBatchRawRecords_MetaSchema = new MetaSchema();
		getBatchRawRecords_MetaSchema.addDataTableSchema(getBatchRawRecords_MetaData1, 5, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRawRecords", parms, getBatchRawRecords_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(4);
		pRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(5);
		RowRaw = (StrongTypesNS.RowRawDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRawRecordsPrev(Rowid pRowid, bool pPrev, int pRowsToReturn, out int pRowsReturned, out StrongTypesNS.RowRawDataTable RowRaw)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(5);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);
		parms.setBooleanParameter(2, pPrev, ParameterSet.INPUT);
		parms.setIntegerParameter(3, pRowsToReturn, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(4, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(5, null, ParameterSet.OUTPUT, "StrongTypesNS.RowRawDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getBatchRawRecordsPrev_MetaSchema = new MetaSchema();
		getBatchRawRecordsPrev_MetaSchema.addDataTableSchema(getBatchRawRecordsPrev_MetaData1, 5, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRawRecordsPrev", parms, getBatchRawRecordsPrev_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(4);
		pRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(5);
		RowRaw = (StrongTypesNS.RowRawDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRecords(Rowid pRowid, bool pNext, int pRowsToReturn, out int pRowsReturned, out StrongTypesNS.RowObjectAuxDataTable RowObjectAux)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(5);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);
		parms.setBooleanParameter(2, pNext, ParameterSet.INPUT);
		parms.setIntegerParameter(3, pRowsToReturn, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(4, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(5, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getBatchRecords_MetaSchema = new MetaSchema();
		getBatchRecords_MetaSchema.addDataTableSchema(getBatchRecords_MetaData1, 5, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRecords", parms, getBatchRecords_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(4);
		pRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(5);
		RowObjectAux = (StrongTypesNS.RowObjectAuxDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRecordsPrev(Rowid pRowid, bool pPrev, int pRowsToReturn, out int pRowsReturned, out StrongTypesNS.RowObjectAuxDataTable RowObjectAux)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(5);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);
		parms.setBooleanParameter(2, pPrev, ParameterSet.INPUT);
		parms.setIntegerParameter(3, pRowsToReturn, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(4, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(5, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getBatchRecordsPrev_MetaSchema = new MetaSchema();
		getBatchRecordsPrev_MetaSchema.addDataTableSchema(getBatchRecordsPrev_MetaData1, 5, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRecordsPrev", parms, getBatchRecordsPrev_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(4);
		pRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(5);
		RowObjectAux = (StrongTypesNS.RowObjectAuxDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getFirst()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getFirst", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getLast()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getLast", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getNext()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getNext", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getPrev()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getPrev", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryStatic(string pQuery)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pQuery, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("openQueryStatic", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string repositionRecord(Rowid pRowid)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("repositionRecord", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string resetQueryParameters()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("resetQueryParameters", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _getTableName()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_getTableName", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _QueryPrepare()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_QueryPrepare", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setIndexedReposition()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setIndexedReposition", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRowids()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRowids", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRowidsInHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRowidsInHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string createRecord()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("createRecord", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string deleteRecord()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("deleteRecord", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string newRecord()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("newRecord", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string updateRecord()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("updateRecord", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string emptyRowObject()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("emptyRowObject", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string emptyRowObjectAux()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("emptyRowObjectAux", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string emptyRowRaw()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("emptyRowRaw", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getAction()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getAction", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string GetBatchRecordsInHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("GetBatchRecordsInHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getDBOXMLExcludeFields()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getDBOXMLExcludeFields", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getExecuteUpdate(out bool lExecute)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(1, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getExecuteUpdate", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		lExecute = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getQueryHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getQueryHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRawRecord(out byte[] pRaw)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setByteParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRawRecord", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pRaw = (byte[])outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getReceivingMessage()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getReceivingMessage", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRecord(out StrongTypesNS.RowObjectAuxDataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getRecord_MetaSchema = new MetaSchema();
		getRecord_MetaSchema.addDataTableSchema(getRecord_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getRecord", parms, getRecord_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowObject = (StrongTypesNS.RowObjectAuxDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string GetRecordInHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("GetRecordInHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRowErrorsHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRowErrorsHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRowid(out Rowid pRowid)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setRowidParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRowid", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pRowid = (Rowid)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRowObjectHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRowObjectHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getTableHandle()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getTableHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLExcludeFields()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLExcludeFields", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLKeyFields()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLKeyFields", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLProducer()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLProducer", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLPublicFields()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLPublicFields", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLSender()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLSender", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLTableName()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLTableName", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLTableNameMult()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLTableNameMult", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLTopic()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getXMLTopic", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string isAvailable()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("isAvailable", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string NewRecordOffQuery(out bool offQuery)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(1, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("NewRecordOffQuery", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		offQuery = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setExecuteUpdate(bool lExecute)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setBooleanParameter(1, lExecute, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setExecuteUpdate", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setRecord(StrongTypesNS.RowObjectAuxDataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables
		MetaSchema setRecord_MetaSchema = new MetaSchema();
		setRecord_MetaSchema.addDataTableSchema(setRecord_MetaData1, 1, ParameterSet.INPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("setRecord", parms, setRecord_MetaSchema);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setRecordHandle(Handle pHdlRowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setHandleParameter(1, pHdlRowObject, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setRecordHandle", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _copyBuffer2TT()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_copyBuffer2TT", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _copyTT2Buffer()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("_copyTT2Buffer", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string compareVersion(string pVersion, out bool pStatus)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pVersion, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(2, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("compareVersion", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pStatus = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string endMethod(string pMethod)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pMethod, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("endMethod", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findFirst()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findFirst", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findLast()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findLast", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findNext(out string pText)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findNext", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pText = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findPrev(out string pText)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findPrev", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pText = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findRowid(Rowid pRowid)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findRowid", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findRowidShow(Rowid pRowid, out StrongTypesNS.RowObjectAuxDataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setRowidParameter(1, pRowid, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema findRowidShow_MetaSchema = new MetaSchema();
		findRowidShow_MetaSchema.addDataTableSchema(findRowidShow_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("findRowidShow", parms, findRowidShow_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		RowObject = (StrongTypesNS.RowObjectAuxDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getCurrent(out StrongTypesNS.RowObjectAuxDataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getCurrent_MetaSchema = new MetaSchema();
		getCurrent_MetaSchema.addDataTableSchema(getCurrent_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getCurrent", parms, getCurrent_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowObject = (StrongTypesNS.RowObjectAuxDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getVersion(out string pVersion)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getVersion", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pVersion = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string serverSendRows(int pStartRow, string pRowid, bool pNext, int pRowsToReturn, out int pRowsReturned, 
out StrongTypesNS.RowObjectAuxDataTable RowObjectAux)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(6);

		// Set up input parameters
		parms.setIntegerParameter(1, pStartRow, ParameterSet.INPUT);
		parms.setStringParameter(2, pRowid, ParameterSet.INPUT);
		parms.setBooleanParameter(3, pNext, ParameterSet.INPUT);
		parms.setIntegerParameter(4, pRowsToReturn, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(5, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(6, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema serverSendRows_MetaSchema = new MetaSchema();
		serverSendRows_MetaSchema.addDataTableSchema(serverSendRows_MetaData1, 6, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("serverSendRows", parms, serverSendRows_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(5);
		pRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(6);
		RowObjectAux = (StrongTypesNS.RowObjectAuxDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setCurrent()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setCurrent", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string startMethod(string pMethod)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pMethod, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("startMethod", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateCreate(StrongTypesNS.RowObjectAuxDataTable RowObject, out StrongTypesNS.RowErrorsDataTable RowErrors, out Rowid pRowid)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectAuxDataTable");


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.RowErrorsDataTable");
		parms.setRowidParameter(3, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables
		MetaSchema validateCreate_MetaSchema = new MetaSchema();
		validateCreate_MetaSchema.addDataTableSchema(validateCreate_MetaData1, 1, ParameterSet.INPUT);
		validateCreate_MetaSchema.addDataTableSchema(validateCreate_MetaData2, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("validateCreate", parms, validateCreate_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		RowErrors = (StrongTypesNS.RowErrorsDataTable)outValue;
		outValue = parms.getOutputParameter(3);
		pRowid = (Rowid)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateDelete(ref Rowid pRowid, out StrongTypesNS.RowErrorsDataTable RowErrors)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters


		// Set up input/output parameters
		parms.setRowidParameter(1, (Rowid)pRowid, ParameterSet.INPUT_OUTPUT);


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.RowErrorsDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema validateDelete_MetaSchema = new MetaSchema();
		validateDelete_MetaSchema.addDataTableSchema(validateDelete_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("validateDelete", parms, validateDelete_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pRowid = (Rowid)outValue;
		outValue = parms.getOutputParameter(2);
		RowErrors = (StrongTypesNS.RowErrorsDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateUpdate(StrongTypesNS.RowObjectAuxDataTable RowObject, Rowid pRowid, out StrongTypesNS.RowErrorsDataTable RowErrors)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectAuxDataTable");
		parms.setRowidParameter(2, pRowid, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(3, null, ParameterSet.OUTPUT, "StrongTypesNS.RowErrorsDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema validateUpdate_MetaSchema = new MetaSchema();
		validateUpdate_MetaSchema.addDataTableSchema(validateUpdate_MetaData1, 1, ParameterSet.INPUT);
		validateUpdate_MetaSchema.addDataTableSchema(validateUpdate_MetaData2, 3, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("validateUpdate", parms, validateUpdate_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(3);
		RowErrors = (StrongTypesNS.RowErrorsDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getCharField(string pFieldName, out string pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getCharField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getDateField(string pFieldName, out System.DateTime pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDateParameter(2, null, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getDateField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (System.DateTime)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getDecField(string pFieldName, out decimal pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDecimalParameter(2, 0.0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getDecField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (decimal)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getIntField(string pFieldName, out int pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(2, 0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getIntField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (int)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getKey(out int pcodEmitente)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(1, 0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getKey", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pcodEmitente = (int)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getLogField(string pFieldName, out bool pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(2, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getLogField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRawField(string pFieldName, out byte[] pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setByteParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRawField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (byte[])outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRecidField(string pFieldName, out long pFieldValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pFieldName, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setLongParameter(2, 0L, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRecidField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pFieldValue = (long)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string goToKey(int pcodEmitente)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, pcodEmitente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("goToKey", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryMain()
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("openQueryMain", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateRecord(string pType)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pType, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("validateRecord", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}



    }


