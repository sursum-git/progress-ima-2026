/*
**
**	Created by PROGRESS ProxyGen (Versão do Progress 11.7) Tue May 24 20:21:32 BRT 2022
**
*/

//
// boad098a
//



    using System;
    using Progress.Open4GL.DynamicAPI;
    using Progress.Open4GL.Proxy;
    using Progress.Open4GL;
    using Progress.Open4GL.Exceptions;

    public class boad098a : Procedure
    {
        // Create a DataTableMetaData object for each temp-table or
        // datatable parm used in any and all methods. Create a
        // DataTableSchema object for each method call that has
        // temp-table or datatable parms which points to one or more
        // temp-tables or datatables used in that method call.


	static DataTableMetaData piRecebeTtMaqEpEstAux_MetaData1;



	static DataTableMetaData getCurrent_MetaData1;



	static DataTableMetaData serverSendRows_MetaData1;



	static DataTableMetaData validateUpdate_MetaData1;

	static DataTableMetaData validateUpdate_MetaData2;



	static DataTableMetaData validateDelete_MetaData1;



	static DataTableMetaData validateCreate_MetaData1;

	static DataTableMetaData validateCreate_MetaData2;



	static DataTableMetaData procuraMaiorMenorDoctoCtaPagar_MetaData1;



	static DataTableMetaData procuraMaiorMenorDoctoCtaReceber_MetaData1;




        static boad098a()
        {
		piRecebeTtMaqEpEstAux_MetaData1 = new DataTableMetaData(0, "ttMaqEpEstAux", 4, false, 0, null, null, null, "StrongTypesNS.ttMaqEpEstAuxDataTable");
		piRecebeTtMaqEpEstAux_MetaData1.setFieldDesc(1, "cd-maquina", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		piRecebeTtMaqEpEstAux_MetaData1.setFieldDesc(2, "ep-codigo", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		piRecebeTtMaqEpEstAux_MetaData1.setFieldDesc(3, "cod-estabel", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		piRecebeTtMaqEpEstAux_MetaData1.setFieldDesc(4, "tp-conexao", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");

		getCurrent_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectDataTable");
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
		getCurrent_MetaData1.setFieldDesc(200, "r-rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		serverSendRows_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectDataTable");
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
		serverSendRows_MetaData1.setFieldDesc(200, "r-rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");

		validateUpdate_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 0, null, null, null, "StrongTypesNS.RowObjectDataTable");
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
		validateUpdate_MetaData1.setFieldDesc(200, "r-rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");
		validateUpdate_MetaData2 = new DataTableMetaData(0, "ttBoErro", 7, false, 0, null, null, null, "StrongTypesNS.ttBoErroDataTable");
		validateUpdate_MetaData2.setFieldDesc(1, "i-sequen", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(2, "cd-erro", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(3, "mensagem", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(4, "parametros", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(5, "errortype", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(6, "errorhelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(7, "errorsubtype", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		validateDelete_MetaData1 = new DataTableMetaData(0, "ttBoErro", 7, false, 0, null, null, null, "StrongTypesNS.ttBoErroDataTable");
		validateDelete_MetaData1.setFieldDesc(1, "i-sequen", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(2, "cd-erro", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(3, "mensagem", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(4, "parametros", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(5, "errortype", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(6, "errorhelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateDelete_MetaData1.setFieldDesc(7, "errorsubtype", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		validateCreate_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 2, "1,cod-emitente:codigo.nome-abrev:nome", null, null, "StrongTypesNS.RowObjectDataTable");
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
		validateCreate_MetaData1.setFieldDesc(200, "r-rowid", 0, Parameter.PRO_ROWID, 0, 199, 0, "", "", 0, null, "");
		validateCreate_MetaData2 = new DataTableMetaData(0, "ttBoErro", 7, false, 0, null, null, null, "StrongTypesNS.ttBoErroDataTable");
		validateCreate_MetaData2.setFieldDesc(1, "i-sequen", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(2, "cd-erro", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(3, "mensagem", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(4, "parametros", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(5, "errortype", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(6, "errorhelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateCreate_MetaData2.setFieldDesc(7, "errorsubtype", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		procuraMaiorMenorDoctoCtaPagar_MetaData1 = new DataTableMetaData(0, "ttCtaPagarRec", 6, false, 0, null, null, null, "StrongTypesNS.ttCtaPagarRecDataTable");
		procuraMaiorMenorDoctoCtaPagar_MetaData1.setFieldDesc(1, "da-maior", 0, Parameter.PRO_DATE, 0, 0, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaPagar_MetaData1.setFieldDesc(2, "de-vl-maior", 0, Parameter.PRO_DECIMAL, 0, 1, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaPagar_MetaData1.setFieldDesc(3, "da-ultimo", 0, Parameter.PRO_DATE, 0, 2, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaPagar_MetaData1.setFieldDesc(4, "de-vl-ultimo", 0, Parameter.PRO_DECIMAL, 0, 3, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaPagar_MetaData1.setFieldDesc(5, "i-atraso-medio", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaPagar_MetaData1.setFieldDesc(6, "r-rowid", 0, Parameter.PRO_ROWID, 0, 5, 0, "", "", 0, null, "");

		procuraMaiorMenorDoctoCtaReceber_MetaData1 = new DataTableMetaData(0, "ttCtaPagarRec", 6, false, 0, null, null, null, "StrongTypesNS.ttCtaPagarRecDataTable");
		procuraMaiorMenorDoctoCtaReceber_MetaData1.setFieldDesc(1, "da-maior", 0, Parameter.PRO_DATE, 0, 0, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaReceber_MetaData1.setFieldDesc(2, "de-vl-maior", 0, Parameter.PRO_DECIMAL, 0, 1, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaReceber_MetaData1.setFieldDesc(3, "da-ultimo", 0, Parameter.PRO_DATE, 0, 2, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaReceber_MetaData1.setFieldDesc(4, "de-vl-ultimo", 0, Parameter.PRO_DECIMAL, 0, 3, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaReceber_MetaData1.setFieldDesc(5, "i-atraso-medio", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		procuraMaiorMenorDoctoCtaReceber_MetaData1.setFieldDesc(6, "r-rowid", 0, Parameter.PRO_ROWID, 0, 5, 0, "", "", 0, null, "");


        }

	//----Constructor

	public boad098a(ProObject appObj) : base(appObj)
	{
                
		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Run procedure
		PersistProcObj = RunPersistentProcedure("boad098a.p", parms);

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
	public decimal fnIndice(int iMoeda)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, iMoeda, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		parms.setDecimalFunction();

		// Run procedure
		base.runProcedure("fn-indice", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		Object retObj = parms.FunctionReturnValue;
		if (retObj == null)
			throw new Open4GLException(BadOutputVal, null);
		return (decimal) retObj;

	}

/// <summary>
	/// 
	/// </summary> 
	public string piMsgAval(int iCodMensagem)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, iCodMensagem, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-msg-aval", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

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
	public string piCriaTtEmitente()
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
		base.runProcedure("pi-cria-tt-emitente", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piCriaTtEmitSafraDis()
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
		base.runProcedure("pi-cria-tt-emit-safra-dis", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piDeterminaLimite(out decimal deLimCredito)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDecimalParameter(1, 0.0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-determina-limite", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		deLimCredito = (decimal)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piLimiteCredSafra(bool pLMatriz, out decimal deLimCredSafra)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setBooleanParameter(1, pLMatriz, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDecimalParameter(2, 0.0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-limite-cred-safra", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		deLimCredSafra = (decimal)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piCreditoAutomatico()
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
		base.runProcedure("pi-credito-automatico", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piVerificaCliente()
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
		base.runProcedure("pi-verifica-cliente", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piVerificaMp()
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
		base.runProcedure("pi-verifica-mp", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piSaldoCr(int iParamAval)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, iParamAval, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-saldo-cr", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piVerificaMesina()
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
		base.runProcedure("pi-verifica-mesina", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piVerificaAtraso()
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
		base.runProcedure("pi-verifica-atraso", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piSaldoAp()
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
		base.runProcedure("pi-saldo-ap", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piSaldoApSafra()
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
		base.runProcedure("pi-saldo-ap-safra", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piVerificaPd()
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
		base.runProcedure("pi-verifica-pd", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piVerificaFt()
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
		base.runProcedure("pi-verifica-ft", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piSetaFator2(int iMoeda)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, iMoeda, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-seta-fator-2", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piConectaEms5(bool lConecta, string pEmpresa, out bool lErroEms5)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setBooleanParameter(1, lConecta, ParameterSet.INPUT);
		parms.setStringParameter(2, pEmpresa, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(3, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-conecta-ems5", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(3);
		lErroEms5 = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piSetaEmpresaEstabel(string iEmprAux, string cCodEstabel)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, iEmprAux, ParameterSet.INPUT);
		parms.setStringParameter(2, cCodEstabel, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-seta-empresa-estabel", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piMonitorTabelas(int pTabela, string pCampos)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setIntegerParameter(1, pTabela, ParameterSet.INPUT);
		parms.setStringParameter(2, pCampos, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-monitor-tabelas", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piRecebeTtMaqEpEstAux(StrongTypesNS.ttMaqEpEstAuxDataTable ttMaqEpEstAux)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setDataTableParameter(1, ttMaqEpEstAux, ParameterSet.INPUT, "StrongTypesNS.ttMaqEpEstAuxDataTable");


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables
		MetaSchema piRecebeTtMaqEpEstAux_MetaSchema = new MetaSchema();
		piRecebeTtMaqEpEstAux_MetaSchema.addDataTableSchema(piRecebeTtMaqEpEstAux_MetaData1, 1, ParameterSet.INPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-recebe-tt-maq-ep-est-aux", parms, piRecebeTtMaqEpEstAux_MetaSchema);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piEnviaEmailCredito(string pNrPedcli, string pNomeAbrev)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pNrPedcli, ParameterSet.INPUT);
		parms.setStringParameter(2, pNomeAbrev, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("pi-envia-email-credito", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint1()
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
		base.runProcedure("setConstraint1", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint2()
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
		base.runProcedure("setConstraint2", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint3(string pCodEmit)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pCodEmit, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint3", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint4(string pCodEmit)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pCodEmit, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint4", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint5(string pCodEmit)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pCodEmit, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint5", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint6(string pNomeAbrev)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pNomeAbrev, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint6", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint7(string pNomeAbrev)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pNomeAbrev, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint7", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint8(string pCgc)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pCgc, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint8", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint10(int pInicial, string pInicial2, string pInicial3, string pInicial4)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(4);

		// Set up input parameters
		parms.setIntegerParameter(1, pInicial, ParameterSet.INPUT);
		parms.setStringParameter(2, pInicial2, ParameterSet.INPUT);
		parms.setStringParameter(3, pInicial3, ParameterSet.INPUT);
		parms.setStringParameter(4, pInicial4, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint10", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraint11(string pCodEmit, string pCodRepres)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pCodEmit, ParameterSet.INPUT);
		parms.setStringParameter(2, pCodRepres, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraint11", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQuery(int iAbertura)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, iAbertura, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("openQuery", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string startMethod(string cMethodName)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, cMethodName, ParameterSet.INPUT);


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
	public string endMethod(string cMethodName)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, cMethodName, ParameterSet.INPUT);


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
	public string compareVersion(string cVersao, out bool lOk)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cVersao, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(2, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("compareVersion", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		lOk = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getVersion(out string cVersao)
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
		cVersao = (string)outValue;


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
	public string findPrev(out string cReturn)
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
		cReturn = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findNext(out string cReturn)
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
		cReturn = (string)outValue;


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
	public string getRowid(out Rowid rChave)
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
		rChave = (Rowid)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findRowid(Rowid rChave)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setRowidParameter(1, rChave, ParameterSet.INPUT);


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
	public string getCurrent(out StrongTypesNS.RowObjectDataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getCurrent_MetaSchema = new MetaSchema();
		getCurrent_MetaSchema.addDataTableSchema(getCurrent_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getCurrent", parms, getCurrent_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowObject = (StrongTypesNS.RowObjectDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string serverSendRows(int piStartRow, string pcRowIdent, bool plNext, int piRowsToReturn, out int piRowsReturned, 
out StrongTypesNS.RowObjectDataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(6);

		// Set up input parameters
		parms.setIntegerParameter(1, piStartRow, ParameterSet.INPUT);
		parms.setStringParameter(2, pcRowIdent, ParameterSet.INPUT);
		parms.setBooleanParameter(3, plNext, ParameterSet.INPUT);
		parms.setIntegerParameter(4, piRowsToReturn, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(5, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(6, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema serverSendRows_MetaSchema = new MetaSchema();
		serverSendRows_MetaSchema.addDataTableSchema(serverSendRows_MetaData1, 6, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("serverSendRows", parms, serverSendRows_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(5);
		piRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(6);
		RowObject = (StrongTypesNS.RowObjectDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string prevBrowseNavigation(string cRowidAtual, int iLinhasPrev, out string cRowidAnt)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setStringParameter(1, cRowidAtual, ParameterSet.INPUT);
		parms.setIntegerParameter(2, iLinhasPrev, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(3, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("prevBrowseNavigation", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(3);
		cRowidAnt = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string executeCreate()
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
		base.runProcedure("executeCreate", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string executeUpdate()
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
		base.runProcedure("executeUpdate", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string executeDelete()
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
		base.runProcedure("executeDelete", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateUpdate(StrongTypesNS.RowObjectDataTable RowObject, Rowid rChave, out StrongTypesNS.ttBoErroDataTable ttBoErro)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectDataTable");
		parms.setRowidParameter(2, rChave, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(3, null, ParameterSet.OUTPUT, "StrongTypesNS.ttBoErroDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema validateUpdate_MetaSchema = new MetaSchema();
		validateUpdate_MetaSchema.addDataTableSchema(validateUpdate_MetaData1, 1, ParameterSet.INPUT);
		validateUpdate_MetaSchema.addDataTableSchema(validateUpdate_MetaData2, 3, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("validateUpdate", parms, validateUpdate_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(3);
		ttBoErro = (StrongTypesNS.ttBoErroDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateDelete(ref Rowid rChave, out StrongTypesNS.ttBoErroDataTable ttBoErro)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters


		// Set up input/output parameters
		parms.setRowidParameter(1, (Rowid)rChave, ParameterSet.INPUT_OUTPUT);


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.ttBoErroDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema validateDelete_MetaSchema = new MetaSchema();
		validateDelete_MetaSchema.addDataTableSchema(validateDelete_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("validateDelete", parms, validateDelete_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		rChave = (Rowid)outValue;
		outValue = parms.getOutputParameter(2);
		ttBoErro = (StrongTypesNS.ttBoErroDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string initializeValidate()
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
		base.runProcedure("initializeValidate", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findCodigo(int iCodEmitente, out string cReturn)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setIntegerParameter(1, iCodEmitente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findCodigo", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cReturn = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findCodigoCliente(int iCodEmitente, out string cReturn)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setIntegerParameter(1, iCodEmitente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findCodigoCliente", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cReturn = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findCodigoFornecedor(int iCodEmitente, out string cReturn)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setIntegerParameter(1, iCodEmitente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findCodigoFornecedor", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cReturn = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string findNome(string cNomeAbrev, out string cReturn)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cNomeAbrev, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("findNome", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cReturn = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateFields()
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
		base.runProcedure("validateFields", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateCreate(StrongTypesNS.RowObjectDataTable RowObject, out StrongTypesNS.ttBoErroDataTable ttBoErro, out Rowid rChave)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectDataTable");


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.ttBoErroDataTable");
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
		ttBoErro = (StrongTypesNS.ttBoErroDataTable)outValue;
		outValue = parms.getOutputParameter(3);
		rChave = (Rowid)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getCharField(string cField, out string cValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cField, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getCharField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cValue = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getDecField(string cField, out decimal cValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cField, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDecimalParameter(2, 0.0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getDecField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cValue = (decimal)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getIntField(string cField, out int cValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cField, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(2, 0, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getIntField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cValue = (int)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getDateField(string cField, out System.DateTime cValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cField, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDateParameter(2, null, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getDateField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cValue = (System.DateTime)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getLogField(string cField, out bool cValue)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cField, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setBooleanParameter(2, false, ParameterSet.OUTPUT, UnknownType.None);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getLogField", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		cValue = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string assinalaFatorConvCtaPagarRec()
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
		base.runProcedure("assinalaFatorConvCtaPagarRec", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string piExecutaUpc()
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
		base.runProcedure("pi-executa-upc", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string procuraMaiorMenorDoctoCtaPagar(Rowid rEmitente, out StrongTypesNS.ttCtaPagarRecDataTable ttCtaPagarRec)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setRowidParameter(1, rEmitente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.ttCtaPagarRecDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema procuraMaiorMenorDoctoCtaPagar_MetaSchema = new MetaSchema();
		procuraMaiorMenorDoctoCtaPagar_MetaSchema.addDataTableSchema(procuraMaiorMenorDoctoCtaPagar_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("procuraMaiorMenorDoctoCtaPagar", parms, procuraMaiorMenorDoctoCtaPagar_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		ttCtaPagarRec = (StrongTypesNS.ttCtaPagarRecDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string procuraMaiorMenorDoctoCtaReceber(Rowid rEmitente, out StrongTypesNS.ttCtaPagarRecDataTable ttCtaPagarRec)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setRowidParameter(1, rEmitente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.ttCtaPagarRecDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema procuraMaiorMenorDoctoCtaReceber_MetaSchema = new MetaSchema();
		procuraMaiorMenorDoctoCtaReceber_MetaSchema.addDataTableSchema(procuraMaiorMenorDoctoCtaReceber_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("procuraMaiorMenorDoctoCtaReceber", parms, procuraMaiorMenorDoctoCtaReceber_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		ttCtaPagarRec = (StrongTypesNS.ttCtaPagarRecDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string geraSituacaoGeralCliente(int pCodEmitente, string pNomeAbrev, int pNrMesina, int pCodGrCli, int pNrPeratr, 
out string pMoeda, out int pTotpdcr, out int pTotpdaa, out int pTotpdre, out int pTotpdap, 
out decimal pLimCredito, out decimal pP5VlCrAbe, out decimal pP6VlApAbe, out decimal pSaldoVw, out decimal pSaldoApVw, 
out decimal pSaldoCrVw, out decimal pVlNotaVw, out decimal pTotvlaaVw, out decimal pTotvlreVw, out decimal pVlPedVw, 
out decimal pTotvlapVw, out decimal pDeTotTit, out string pSinal)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(23);

		// Set up input parameters
		parms.setIntegerParameter(1, pCodEmitente, ParameterSet.INPUT);
		parms.setStringParameter(2, pNomeAbrev, ParameterSet.INPUT);
		parms.setIntegerParameter(3, pNrMesina, ParameterSet.INPUT);
		parms.setIntegerParameter(4, pCodGrCli, ParameterSet.INPUT);
		parms.setIntegerParameter(5, pNrPeratr, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(6, null, ParameterSet.OUTPUT);
		parms.setIntegerParameter(7, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setIntegerParameter(8, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setIntegerParameter(9, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setIntegerParameter(10, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(11, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(12, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(13, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(14, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(15, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(16, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(17, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(18, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(19, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(20, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(21, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(22, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setStringParameter(23, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("geraSituacaoGeralCliente", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(6);
		pMoeda = (string)outValue;
		outValue = parms.getOutputParameter(7);
		pTotpdcr = (int)outValue;
		outValue = parms.getOutputParameter(8);
		pTotpdaa = (int)outValue;
		outValue = parms.getOutputParameter(9);
		pTotpdre = (int)outValue;
		outValue = parms.getOutputParameter(10);
		pTotpdap = (int)outValue;
		outValue = parms.getOutputParameter(11);
		pLimCredito = (decimal)outValue;
		outValue = parms.getOutputParameter(12);
		pP5VlCrAbe = (decimal)outValue;
		outValue = parms.getOutputParameter(13);
		pP6VlApAbe = (decimal)outValue;
		outValue = parms.getOutputParameter(14);
		pSaldoVw = (decimal)outValue;
		outValue = parms.getOutputParameter(15);
		pSaldoApVw = (decimal)outValue;
		outValue = parms.getOutputParameter(16);
		pSaldoCrVw = (decimal)outValue;
		outValue = parms.getOutputParameter(17);
		pVlNotaVw = (decimal)outValue;
		outValue = parms.getOutputParameter(18);
		pTotvlaaVw = (decimal)outValue;
		outValue = parms.getOutputParameter(19);
		pTotvlreVw = (decimal)outValue;
		outValue = parms.getOutputParameter(20);
		pVlPedVw = (decimal)outValue;
		outValue = parms.getOutputParameter(21);
		pTotvlapVw = (decimal)outValue;
		outValue = parms.getOutputParameter(22);
		pDeTotTit = (decimal)outValue;
		outValue = parms.getOutputParameter(23);
		pSinal = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string geraSituacaoGeralGrupoCliente(int pCodEmitente, string pNomeAbrev, int pNrMesina, int pCodGrCli, int pNrPeratr, 
int pIndAbrangeAval, string pNomeMatriz, out string pMoeda, out int pTotpdcr, out int pTotpdaa, 
out int pTotpdre, out int pTotpdap, out decimal pLimCredito, out decimal pP5VlCrAbe, out decimal pP6VlApAbe, 
out decimal pSaldoVw, out decimal pSaldoApVw, out decimal pSaldoCrVw, out decimal pVlNotaVw, out decimal pTotvlaaVw, 
out decimal pTotvlreVw, out decimal pVlPedVw, out decimal pTotvlapVw, out string pSinal)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(24);

		// Set up input parameters
		parms.setIntegerParameter(1, pCodEmitente, ParameterSet.INPUT);
		parms.setStringParameter(2, pNomeAbrev, ParameterSet.INPUT);
		parms.setIntegerParameter(3, pNrMesina, ParameterSet.INPUT);
		parms.setIntegerParameter(4, pCodGrCli, ParameterSet.INPUT);
		parms.setIntegerParameter(5, pNrPeratr, ParameterSet.INPUT);
		parms.setIntegerParameter(6, pIndAbrangeAval, ParameterSet.INPUT);
		parms.setStringParameter(7, pNomeMatriz, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setStringParameter(8, null, ParameterSet.OUTPUT);
		parms.setIntegerParameter(9, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setIntegerParameter(10, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setIntegerParameter(11, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setIntegerParameter(12, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(13, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(14, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(15, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(16, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(17, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(18, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(19, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(20, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(21, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(22, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDecimalParameter(23, 0.0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setStringParameter(24, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("geraSituacaoGeralGrupoCliente", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(8);
		pMoeda = (string)outValue;
		outValue = parms.getOutputParameter(9);
		pTotpdcr = (int)outValue;
		outValue = parms.getOutputParameter(10);
		pTotpdaa = (int)outValue;
		outValue = parms.getOutputParameter(11);
		pTotpdre = (int)outValue;
		outValue = parms.getOutputParameter(12);
		pTotpdap = (int)outValue;
		outValue = parms.getOutputParameter(13);
		pLimCredito = (decimal)outValue;
		outValue = parms.getOutputParameter(14);
		pP5VlCrAbe = (decimal)outValue;
		outValue = parms.getOutputParameter(15);
		pP6VlApAbe = (decimal)outValue;
		outValue = parms.getOutputParameter(16);
		pSaldoVw = (decimal)outValue;
		outValue = parms.getOutputParameter(17);
		pSaldoApVw = (decimal)outValue;
		outValue = parms.getOutputParameter(18);
		pSaldoCrVw = (decimal)outValue;
		outValue = parms.getOutputParameter(19);
		pVlNotaVw = (decimal)outValue;
		outValue = parms.getOutputParameter(20);
		pTotvlaaVw = (decimal)outValue;
		outValue = parms.getOutputParameter(21);
		pTotvlreVw = (decimal)outValue;
		outValue = parms.getOutputParameter(22);
		pVlPedVw = (decimal)outValue;
		outValue = parms.getOutputParameter(23);
		pTotvlapVw = (decimal)outValue;
		outValue = parms.getOutputParameter(24);
		pSinal = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}



    }


