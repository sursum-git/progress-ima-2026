
/*
**
*/

//
// ttParam - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttParamDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttParam" + "DataSet";
            ds.Namespace = "ttParam" + "NS";

            
	    DataTable ttParam = ds.Tables.Add("ttParam");
	    ttParam.Columns.Add("cod-estab-ini", typeof(string));
	    ttParam.Columns.Add("cod-estab-fim", typeof(string));
	    ttParam.Columns.Add("cod-esp-ini", typeof(string));
	    ttParam.Columns.Add("cod-esp-fim", typeof(string));
	    ttParam.Columns.Add("cod-port-ini", typeof(int));
	    ttParam.Columns.Add("cod-port-fim", typeof(int));
	    ttParam.Columns.Add("dt-emissao-ini", typeof(System.DateTime));
	    ttParam.Columns.Add("dt-emissao-fim", typeof(System.DateTime));


            ds.WriteXmlSchema("ttParam.xsd");

        }


    }
}
