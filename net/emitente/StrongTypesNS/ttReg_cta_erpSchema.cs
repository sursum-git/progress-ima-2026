
/*
**
*/

//
// ttReg_cta_erp - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttReg_cta_erpDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttReg_cta_erp" + "DataSet";
            ds.Namespace = "ttReg_cta_erp" + "NS";

            
	    DataTable ttReg_cta_erp = ds.Tables.Add("ttReg_cta_erp");
	    ttReg_cta_erp.Columns.Add("cod_mic_reg", typeof(string));


            ds.WriteXmlSchema("ttReg_cta_erp.xsd");

        }


    }
}
