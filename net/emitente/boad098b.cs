/*
**
**	Created by PROGRESS ProxyGen (Versão do Progress 11.7) Tue May 24 20:21:32 BRT 2022
**
*/

//
// boad098b
//



    using System;
    using Progress.Open4GL.DynamicAPI;
    using Progress.Open4GL.Proxy;
    using Progress.Open4GL;
    using Progress.Open4GL.Exceptions;

    public class boad098b : Procedure
    {
        // Create a DataTableMetaData object for each temp-table or
        // datatable parm used in any and all methods. Create a
        // DataTableSchema object for each method call that has
        // temp-table or datatable parms which points to one or more
        // temp-tables or datatables used in that method call.


	static DataTableMetaData serverSendRows_MetaData1;




        static boad098b()
        {
		serverSendRows_MetaData1 = new DataTableMetaData(0, "RowObject", 200, false, 2, "1,cod-emitente:codigo.nome-abrev:nome", null, null, "StrongTypesNS.RowObjectDataTable");
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


        }

	//----Constructor

	public boad098b(ProObject appObj) : base(appObj)
	{
                
		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Run procedure
		PersistProcObj = RunPersistentProcedure("boad098b.p", parms);

		// Get output parameters



	}

        /// <summary>
	/// 
	/// </summary> 
	public string setConstraint(Rowid pRowid)
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
		base.runProcedure("setConstraint", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQuery()
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
		base.runProcedure("openQuery", parms);

		// Get output parameters


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



    }


