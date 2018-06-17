/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package dbmsproject;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 *
 * @Vivek Singh
 */
public class BusPass {
    private static final String DB = "jdbc:mysql://localhost/dbms";
    private static final String USER = "root";
    private static final String PASS = "";
    
    
    public static MainFrame obj;
    public static Insertdata obj2;
    public static Updatedata obj3;
    public static Searchdata obj4;
    public static Deletedata obj5;
    public static Connection con;
    
    public static void connectToDatabase() throws ClassNotFoundException, SQLException
    {
        try
        {
            Class.forName("com.mysql.jdbc.Driver");
            con = DriverManager.getConnection(DB, USER, PASS);
        }
        catch(SQLException e)
        {
            e.printStackTrace();
        }
    }
    
    public static void main(String[] args) throws ClassNotFoundException, SQLException {
        obj = new MainFrame();
        obj2 = new InsertFrame();
        obj3 = new UpdateFrame();
        obj4 = new SearchFrame();
        obj5 = new DeleteFrame();
        obj2.initializeComboBox();
        obj3.initializeComboBox();
        obj.setVisible(true);
        connectToDatabase();
    }
    
}
