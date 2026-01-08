/*
**
**	Created by PROGRESS ProxyGen (Versão do Progress 11.7) Tue May 24 20:21:32 BRT 2022
**
*/

//
// boad098na2
//



    using System;
    using Progress.Open4GL.DynamicAPI;
    using Progress.Open4GL.Proxy;
    using Progress.Open4GL;
    using Progress.Open4GL.Exceptions;

    public class boad098na2 : Procedure
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



	static DataTableMetaData getBatchRowids_MetaData1;



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



	static DataTableMetaData ReturnCustomerSalesChannels_MetaData1;



	static DataTableMetaData ReturnsCustumerListRegion_MetaData1;



	static DataTableMetaData ReturnAccountRegion_MetaData1;




        static boad098na2()
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

		getBatchRecords_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		getBatchRecords_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getBatchRecords_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		getBatchRecordsPrev_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		getBatchRecordsPrev_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getBatchRecordsPrev_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		getBatchRowids_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		getBatchRowids_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getBatchRowids_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		getRecord_MetaData1 = new DataTableMetaData(0, "RowObject", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		getRecord_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getRecord_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		setRecord_MetaData1 = new DataTableMetaData(0, "RowObject", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		setRecord_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		setRecord_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		findRowidShow_MetaData1 = new DataTableMetaData(0, "RowObject", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		findRowidShow_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		findRowidShow_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		getCurrent_MetaData1 = new DataTableMetaData(0, "RowObject", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		getCurrent_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		getCurrent_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		serverSendRows_MetaData1 = new DataTableMetaData(0, "RowObjectAux", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		serverSendRows_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		serverSendRows_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");

		validateCreate_MetaData1 = new DataTableMetaData(0, "RowObject", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		validateCreate_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		validateCreate_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
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

		validateUpdate_MetaData1 = new DataTableMetaData(0, "RowObject", 9, false, 0, null, null, null, "StrongTypesNS.RowObjectAux2DataTable");
		validateUpdate_MetaData1.setFieldDesc(1, "r-rowid", 0, Parameter.PRO_ROWID, 0, 0, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(2, "cgc", 0, Parameter.PRO_CHARACTER, 0, 1, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(3, "cod-emitente", 0, Parameter.PRO_INTEGER, 0, 2, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(4, "cod-gr-forn", 0, Parameter.PRO_INTEGER, 0, 3, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(5, "identific", 0, Parameter.PRO_INTEGER, 0, 4, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(6, "natureza", 0, Parameter.PRO_INTEGER, 0, 5, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(7, "nome-abrev", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(8, "nome-emit", 0, Parameter.PRO_CHARACTER, 0, 7, 0, "", "", 0, null, "");
		validateUpdate_MetaData1.setFieldDesc(9, "nome-matriz", 0, Parameter.PRO_CHARACTER, 0, 8, 0, "", "", 0, null, "");
		validateUpdate_MetaData2 = new DataTableMetaData(0, "RowErrors", 7, false, 0, null, null, null, "StrongTypesNS.RowErrorsDataTable");
		validateUpdate_MetaData2.setFieldDesc(1, "ErrorSequence", 0, Parameter.PRO_INTEGER, 0, 0, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(2, "ErrorNumber", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(3, "ErrorDescription", 0, Parameter.PRO_CHARACTER, 0, 2, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(4, "ErrorParameters", 0, Parameter.PRO_CHARACTER, 0, 3, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(5, "ErrorType", 0, Parameter.PRO_CHARACTER, 0, 4, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(6, "ErrorHelp", 0, Parameter.PRO_CHARACTER, 0, 5, 0, "", "", 0, null, "");
		validateUpdate_MetaData2.setFieldDesc(7, "ErrorSubType", 0, Parameter.PRO_CHARACTER, 0, 6, 0, "", "", 0, null, "");

		ReturnCustomerSalesChannels_MetaData1 = new DataTableMetaData(0, "ttCta_canal_clien", 1, false, 0, null, null, null, "StrongTypesNS.ttCta_canal_clienDataTable");
		ReturnCustomerSalesChannels_MetaData1.setFieldDesc(1, "cod_pessoa_erp", 0, Parameter.PRO_CHARACTER, 0, 0, 0, "", "", 0, null, "");

		ReturnsCustumerListRegion_MetaData1 = new DataTableMetaData(0, "ttAccount", 2, false, 0, null, null, null, "StrongTypesNS.ttAccountDataTable");
		ReturnsCustumerListRegion_MetaData1.setFieldDesc(1, "cod_pessoa_erp", 0, Parameter.PRO_CHARACTER, 0, 0, 0, "", "", 0, null, "");
		ReturnsCustumerListRegion_MetaData1.setFieldDesc(2, "num_id_pessoa", 0, Parameter.PRO_INTEGER, 0, 1, 0, "", "", 0, null, "");

		ReturnAccountRegion_MetaData1 = new DataTableMetaData(0, "ttReg_cta_erp", 1, false, 0, null, null, null, "StrongTypesNS.ttReg_cta_erpDataTable");
		ReturnAccountRegion_MetaData1.setFieldDesc(1, "cod_mic_reg", 0, Parameter.PRO_CHARACTER, 0, 0, 0, "", "", 0, null, "");


        }

	//----Constructor

	public boad098na2(ProObject appObj) : base(appObj)
	{
                
		ParameterSet parms = new ParameterSet(0);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Run procedure
		PersistProcObj = RunPersistentProcedure("boad098na2.p", parms);

		// Get output parameters



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
	public string findRecordOFRowObject()
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
		base.runProcedure("findRecordOFRowObject", parms);

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
	public string getBatchRecords(Rowid pRowid, bool pNext, int pRowsToReturn, out int pRowsReturned, out StrongTypesNS.RowObjectAux2DataTable RowObjectAux)
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
		parms.setDataTableParameter(5, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


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
		RowObjectAux = (StrongTypesNS.RowObjectAux2DataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRecordsPrev(Rowid pRowid, bool pPrev, int pRowsToReturn, out int pRowsReturned, out StrongTypesNS.RowObjectAux2DataTable RowObjectAux)
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
		parms.setDataTableParameter(5, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


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
		RowObjectAux = (StrongTypesNS.RowObjectAux2DataTable)outValue;


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
	public string openQueryDynamic()
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
		base.runProcedure("openQueryDynamic", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string reposisionQuery(int rownum)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setIntegerParameter(1, rownum, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("reposisionQuery", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string countQuery(out int iCont)
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
		base.runProcedure("countQuery", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		iCont = (int)outValue;


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
	public string setQueryBy(string pBYClause)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pBYClause, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setQueryBy", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setQueryFieldList(string pFieldList)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pFieldList, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setQueryFieldList", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setQueryWhere(string pWhereClause)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setStringParameter(1, pWhereClause, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setQueryWhere", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string _getTableName(out string pTableName)
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
		base.runProcedure("_getTableName", parms);

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
	public string _QueryPrepare(out string pQueryPrepareClause)
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
		base.runProcedure("_QueryPrepare", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pQueryPrepareClause = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setIndexedReposition(bool indexRepo)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setBooleanParameter(1, indexRepo, ParameterSet.INPUT);


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
	public string getBatchRowids(out int pRowsReturned, out StrongTypesNS.RowObjectAux2DataTable RowObjectAux)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(1, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getBatchRowids_MetaSchema = new MetaSchema();
		getBatchRowids_MetaSchema.addDataTableSchema(getBatchRowids_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRowids", parms, getBatchRowids_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		pRowsReturned = (int)outValue;
		outValue = parms.getOutputParameter(2);
		RowObjectAux = (StrongTypesNS.RowObjectAux2DataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getBatchRowidsInHandle(out int iReturnedRows, out Handle hRowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(1, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setHandleParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getBatchRowidsInHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		iReturnedRows = (int)outValue;
		outValue = parms.getOutputParameter(2);
		hRowObject = (Handle)outValue;


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
	public string receiveMessage(Handle pReceiveMessage, out Handle pReturnMessage)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setHandleParameter(1, pReceiveMessage, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setHandleParameter(2, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("receiveMessage", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		pReturnMessage = (Handle)outValue;


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
	public string getAction(out string cAction)
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
		base.runProcedure("getAction", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cAction = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string GetBatchRecordsInHandle(Rowid rRowid, bool lNext, int iMaxRows, out int iReturnedRows, out Handle hRowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(5);

		// Set up input parameters
		parms.setRowidParameter(1, rRowid, ParameterSet.INPUT);
		parms.setBooleanParameter(2, lNext, ParameterSet.INPUT);
		parms.setIntegerParameter(3, iMaxRows, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setIntegerParameter(4, 0, ParameterSet.OUTPUT, UnknownType.None);
		parms.setHandleParameter(5, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("GetBatchRecordsInHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(4);
		iReturnedRows = (int)outValue;
		outValue = parms.getOutputParameter(5);
		hRowObject = (Handle)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getDBOXMLExcludeFields(out string cDBOExcludeFields)
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
		base.runProcedure("getDBOXMLExcludeFields", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cDBOExcludeFields = (string)outValue;


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
	public string getQueryHandle(out Handle hQuery)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setHandleParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getQueryHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		hQuery = (Handle)outValue;


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
	public string getReceivingMessage(out bool lStatus)
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
		base.runProcedure("getReceivingMessage", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		lStatus = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRecord(out StrongTypesNS.RowObjectAux2DataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getRecord_MetaSchema = new MetaSchema();
		getRecord_MetaSchema.addDataTableSchema(getRecord_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getRecord", parms, getRecord_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowObject = (StrongTypesNS.RowObjectAux2DataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string GetRecordInHandle(out Handle hRowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setHandleParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("GetRecordInHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		hRowObject = (Handle)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getRowErrorsHandle(out Handle hRowErrors)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setHandleParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRowErrorsHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		hRowErrors = (Handle)outValue;


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
	public string getRowObjectHandle(out Handle hRowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setHandleParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getRowObjectHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		hRowObject = (Handle)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getTableHandle(out Handle hTable)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setHandleParameter(1, null, ParameterSet.OUTPUT);


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("getTableHandle", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		hTable = (Handle)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLExcludeFields(out string cExcludeFields)
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
		base.runProcedure("getXMLExcludeFields", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cExcludeFields = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLKeyFields(out string cKeyFields)
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
		base.runProcedure("getXMLKeyFields", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cKeyFields = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLProducer(out bool lXMLProducer)
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
		base.runProcedure("getXMLProducer", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		lXMLProducer = (bool)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLPublicFields(out string cPublicFields)
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
		base.runProcedure("getXMLPublicFields", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cPublicFields = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLSender(out string cXMLSender)
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
		base.runProcedure("getXMLSender", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cXMLSender = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLTableName(out string cXMLTableName)
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
		base.runProcedure("getXMLTableName", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cXMLTableName = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLTableNameMult(out string cXMLTableNameMult)
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
		base.runProcedure("getXMLTableNameMult", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cXMLTableNameMult = (string)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getXMLTopic(out string cXMLTopic)
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
		base.runProcedure("getXMLTopic", parms);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		cXMLTopic = (string)outValue;


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
	public string setReceivingMessage(bool lStatus)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setBooleanParameter(1, lStatus, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setReceivingMessage", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setRecord(StrongTypesNS.RowObjectAux2DataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectAux2DataTable");


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
	public string findRowidShow(Rowid pRowid, out StrongTypesNS.RowObjectAux2DataTable RowObject)
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
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema findRowidShow_MetaSchema = new MetaSchema();
		findRowidShow_MetaSchema.addDataTableSchema(findRowidShow_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("findRowidShow", parms, findRowidShow_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		RowObject = (StrongTypesNS.RowObjectAux2DataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string getCurrent(out StrongTypesNS.RowObjectAux2DataTable RowObject)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(1);

		// Set up input parameters


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(1, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema getCurrent_MetaSchema = new MetaSchema();
		getCurrent_MetaSchema.addDataTableSchema(getCurrent_MetaData1, 1, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("getCurrent", parms, getCurrent_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(1);
		RowObject = (StrongTypesNS.RowObjectAux2DataTable)outValue;


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
out StrongTypesNS.RowObjectAux2DataTable RowObjectAux)
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
		parms.setDataTableParameter(6, null, ParameterSet.OUTPUT, "StrongTypesNS.RowObjectAux2DataTable");


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
		RowObjectAux = (StrongTypesNS.RowObjectAux2DataTable)outValue;


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
	public string validateCreate(StrongTypesNS.RowObjectAux2DataTable RowObject, out StrongTypesNS.RowErrorsDataTable RowErrors, out Rowid pRowid)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectAux2DataTable");


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
	public string validateUpdate(StrongTypesNS.RowObjectAux2DataTable RowObject, Rowid pRowid, out StrongTypesNS.RowErrorsDataTable RowErrors)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setDataTableParameter(1, RowObject, ParameterSet.INPUT, "StrongTypesNS.RowObjectAux2DataTable");
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
	public string openQueryCGC()
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
		base.runProcedure("openQueryCGC", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryCodEmitente()
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
		base.runProcedure("openQueryCodEmitente", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryGrupo()
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
		base.runProcedure("openQueryGrupo", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryIniciaCom()
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
		base.runProcedure("openQueryIniciaCom", parms);

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
	public string openQueryNomeAbrev()
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
		base.runProcedure("openQueryNomeAbrev", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryNomeEmitente()
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
		base.runProcedure("openQueryNomeEmitente", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string openQueryNomeMatriz()
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
		base.runProcedure("openQueryNomeMatriz", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintCGC(string pCgcIni, string pCgcFim)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pCgcIni, ParameterSet.INPUT);
		parms.setStringParameter(2, pCgcFim, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintCGC", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintCodEmitente(int pCodEmitenteIni, int pCodEmitenteFim)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setIntegerParameter(1, pCodEmitenteIni, ParameterSet.INPUT);
		parms.setIntegerParameter(2, pCodEmitenteFim, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintCodEmitente", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintGrupo(int pCodGrFornIni, int pCodGrFornFim)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setIntegerParameter(1, pCodGrFornIni, ParameterSet.INPUT);
		parms.setIntegerParameter(2, pCodGrFornFim, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintGrupo", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintIniciaCom(string pIniciacom, int pOpcaoIniciacom)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pIniciacom, ParameterSet.INPUT);
		parms.setIntegerParameter(2, pOpcaoIniciacom, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintIniciaCom", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintMain()
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
		base.runProcedure("setConstraintMain", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintNomeAbrev(string pNomeAbrevIni, string pNomeAbrevFim)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pNomeAbrevIni, ParameterSet.INPUT);
		parms.setStringParameter(2, pNomeAbrevFim, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintNomeAbrev", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintNomeEmitente(string pNomeEmitIni, string pNomeEmitFim)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pNomeEmitIni, ParameterSet.INPUT);
		parms.setStringParameter(2, pNomeEmitFim, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintNomeEmitente", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string setConstraintNomeMatriz(string pNomeMatrizIni, string pNomeMatrizFim)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pNomeMatrizIni, ParameterSet.INPUT);
		parms.setStringParameter(2, pNomeMatrizFim, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters


		// Setup local MetaSchema if any params are tables



		// Set up return type
		

		// Run procedure
		base.runProcedure("setConstraintNomeMatriz", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string todosEmitentes()
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
		base.runProcedure("todosEmitentes", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string validateDeleteFrotas()
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
		base.runProcedure("validateDeleteFrotas", parms);

		// Get output parameters


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string ReturnCustomerSalesChannels(string cod_canal_cliente, out StrongTypesNS.ttCta_canal_clienDataTable ttCta_canal_clien)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, cod_canal_cliente, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.ttCta_canal_clienDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema ReturnCustomerSalesChannels_MetaSchema = new MetaSchema();
		ReturnCustomerSalesChannels_MetaSchema.addDataTableSchema(ReturnCustomerSalesChannels_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("ReturnCustomerSalesChannels", parms, ReturnCustomerSalesChannels_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		ttCta_canal_clien = (StrongTypesNS.ttCta_canal_clienDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string ReturnsCustumerListRegion(string pcCod_regiao_erp, int piIdi_niv_acess, out StrongTypesNS.ttAccountDataTable ttAccount)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(3);

		// Set up input parameters
		parms.setStringParameter(1, pcCod_regiao_erp, ParameterSet.INPUT);
		parms.setIntegerParameter(2, piIdi_niv_acess, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(3, null, ParameterSet.OUTPUT, "StrongTypesNS.ttAccountDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema ReturnsCustumerListRegion_MetaSchema = new MetaSchema();
		ReturnsCustumerListRegion_MetaSchema.addDataTableSchema(ReturnsCustumerListRegion_MetaData1, 3, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("ReturnsCustumerListRegion", parms, ReturnsCustumerListRegion_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(3);
		ttAccount = (StrongTypesNS.ttAccountDataTable)outValue;


		if (rqCtx != null) rqCtx.Release();


		// Return output value
		return (string)(parms.ProcedureReturnValue);

	}

/// <summary>
	/// 
	/// </summary> 
	public string ReturnAccountRegion(string pcCod_pessoa_erp, out StrongTypesNS.ttReg_cta_erpDataTable ttReg_cta_erp)
	{
		RqContext rqCtx = null;
		if (isSessionAvailable() == false)
			throw new Open4GLException(NotAvailable, null);

		Object outValue;
		ParameterSet parms = new ParameterSet(2);

		// Set up input parameters
		parms.setStringParameter(1, pcCod_pessoa_erp, ParameterSet.INPUT);


		// Set up input/output parameters


		// Set up Out parameters
		parms.setDataTableParameter(2, null, ParameterSet.OUTPUT, "StrongTypesNS.ttReg_cta_erpDataTable");


		// Setup local MetaSchema if any params are tables
		MetaSchema ReturnAccountRegion_MetaSchema = new MetaSchema();
		ReturnAccountRegion_MetaSchema.addDataTableSchema(ReturnAccountRegion_MetaData1, 2, ParameterSet.OUTPUT);


		// Set up return type
		

		// Run procedure
		base.runProcedure("ReturnAccountRegion", parms, ReturnAccountRegion_MetaSchema);

		// Get output parameters
		outValue = parms.getOutputParameter(2);
		ttReg_cta_erp = (StrongTypesNS.ttReg_cta_erpDataTable)outValue;


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


