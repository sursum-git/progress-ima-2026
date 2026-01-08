
/*
**
*/

//
// RowInfo - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowInfoDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowInfo" + "DataSet";
            ds.Namespace = "RowInfo" + "NS";

            
	    DataTable RowInfo = ds.Tables.Add("RowInfo");
	    RowInfo.Columns.Add("Code", typeof(string));
	    RowInfo.Columns.Add("CodeValue", typeof(string));


            ds.WriteXmlSchema("RowInfo.xsd");

        }


    }
}
