/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package dbmsproject;

import static dbmsproject.DBMSProject.con;
import static dbmsproject.DBMSProject.obj;
import static dbmsproject.DBMSProject.obj3;
import static java.lang.Integer.parseInt;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JOptionPane;

/**
 *
 * @author Ashish
 */
public class UpdateFrame extends javax.swing.JFrame {

    /**
     * Creates new form UpdateFrame
     */
    public UpdateFrame() {
        initComponents();
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        insertAgeGroup = new javax.swing.ButtonGroup();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        insertIDField = new javax.swing.JTextField();
        jLabel3 = new javax.swing.JLabel();
        insertNameField = new javax.swing.JTextField();
        jLabel4 = new javax.swing.JLabel();
        jLabel5 = new javax.swing.JLabel();
        insertAgeCombo = new javax.swing.JComboBox();
        jLabel6 = new javax.swing.JLabel();
        insertSearchButton = new javax.swing.JButton();
        jButton2 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        insertMaleButton = new javax.swing.JRadioButton();
        insertFemaleButton = new javax.swing.JRadioButton();
        jLabel7 = new javax.swing.JLabel();
        insertSubjectField = new javax.swing.JTextField();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jLabel1.setText("Enter student ID whose data is to be updated:");

        jLabel2.setText("ID:");

        insertIDField.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertIDFieldActionPerformed(evt);
            }
        });

        jLabel3.setText("Enter data to be updated:");

        jLabel4.setText("Name:");

        jLabel5.setText("Age:");

        insertAgeCombo.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "18", "19", "20" }));
        insertAgeCombo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertAgeComboActionPerformed(evt);
            }
        });

        jLabel6.setText("Gender:");

        insertSearchButton.setText("Search");
        insertSearchButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertSearchButtonActionPerformed(evt);
            }
        });

        jButton2.setText("Cancel");
        jButton2.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton2ActionPerformed(evt);
            }
        });

        jButton3.setText("Enter");
        jButton3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton3ActionPerformed(evt);
            }
        });

        insertAgeGroup.add(insertMaleButton);
        insertMaleButton.setText("Male");

        insertAgeGroup.add(insertFemaleButton);
        insertFemaleButton.setText("Female");

        jLabel7.setText("Subject");

        insertSubjectField.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                insertSubjectFieldActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addGap(10, 10, 10)
                                .addComponent(jLabel2, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(insertIDField, javax.swing.GroupLayout.PREFERRED_SIZE, 113, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 250, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel3, javax.swing.GroupLayout.PREFERRED_SIZE, 250, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGroup(layout.createSequentialGroup()
                                .addGap(13, 13, 13)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addGroup(layout.createSequentialGroup()
                                        .addComponent(jLabel7, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(insertSubjectField))
                                    .addGroup(layout.createSequentialGroup()
                                        .addComponent(jLabel5, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(insertAgeCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                    .addGroup(layout.createSequentialGroup()
                                        .addComponent(jLabel4, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(insertNameField))
                                    .addGroup(layout.createSequentialGroup()
                                        .addComponent(jLabel6, javax.swing.GroupLayout.PREFERRED_SIZE, 59, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(insertMaleButton, javax.swing.GroupLayout.PREFERRED_SIZE, 58, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(insertFemaleButton)))))
                        .addGap(0, 129, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addComponent(jButton3)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButton2))
                            .addComponent(insertSearchButton, javax.swing.GroupLayout.Alignment.TRAILING))))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(24, 24, 24)
                .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 22, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(insertIDField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(insertSearchButton)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel3, javax.swing.GroupLayout.PREFERRED_SIZE, 22, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(insertNameField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel4))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(insertAgeCombo, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(insertMaleButton)
                    .addComponent(insertFemaleButton))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel7)
                    .addComponent(insertSubjectField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButton2)
                    .addComponent(jButton3))
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton2ActionPerformed
        (obj3).setVisible(false);
        (obj).setVisible(true);
    }//GEN-LAST:event_jButton2ActionPerformed

    public void initializeComboBox()
    {
        DefaultComboBoxModel model = (DefaultComboBoxModel) insertAgeCombo.getModel();
        for(int i = 21; i <= 60; ++i) // here we are adding ages in our age combobox from 21 to 60. 18,19,20 are there by default
        {
            model.addElement(i);
        }
    }
    
    private void insertSearchButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertSearchButtonActionPerformed
        if(insertIDField.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null,"Name cannot be left empty!","Error",JOptionPane.WARNING_MESSAGE);
            return;
        }
        
        int id = 0;
        try
        {
            id = parseInt(insertIDField.getText());
        }
        catch(NumberFormatException e) // if it is not an integer
        {
            JOptionPane.showMessageDialog(null,"Please enter number as ID!","Error",JOptionPane.WARNING_MESSAGE);
        }
        
        try 
        {
            int count = 0;
            PreparedStatement stmt = con.prepareStatement("SELECT * FROM mytable WHERE id = ?");
            stmt.setInt(1, id);
            ResultSet rs = stmt.executeQuery();
            while(rs.next())
            {
                System.out.println(rs.getString("name"));
                insertNameField.setText(rs.getString("name"));
                System.out.println("h");
                insertAgeCombo.setSelectedItem(rs.getInt("age"));
                System.out.println("h");
                if(rs.getString("gender").equals("Male"))
                {
                    insertMaleButton.setSelected(true);
                }
                else
                {
                    insertFemaleButton.setSelected(true);
                }
                insertSubjectField.setText(rs.getString("subject"));
                ++count;
            }
            
            if(count == 0)
            {
                JOptionPane.showMessageDialog(null,"No data found!","Error",JOptionPane.WARNING_MESSAGE);
            }
        } 
        catch (SQLException ex) 
        {
            Logger.getLogger(UpdateFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_insertSearchButtonActionPerformed

    private void insertIDFieldActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertIDFieldActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_insertIDFieldActionPerformed

    private void insertAgeComboActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertAgeComboActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_insertAgeComboActionPerformed

    private void insertSubjectFieldActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_insertSubjectFieldActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_insertSubjectFieldActionPerformed

    private void jButton3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton3ActionPerformed
        
        if(insertNameField.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null,"Name cannot be left empty!","Error",JOptionPane.WARNING_MESSAGE);
            return;
        }
        if(insertSubjectField.getText().equals(""))
        {
            JOptionPane.showMessageDialog(null,"Subject cannot be left empty!","Error",JOptionPane.WARNING_MESSAGE);
            return;
        }
        if(insertAgeGroup.getSelection() == null)
        {
            JOptionPane.showMessageDialog(null,"Age cannot be left empty!","Error",JOptionPane.WARNING_MESSAGE);
            return;
        }
        
        int id = 0;
        try
        {
            id = parseInt(insertIDField.getText());
        }
        catch(NumberFormatException e) // if it is not an integer
        {
            JOptionPane.showMessageDialog(null,"Please enter number as ID!","Error",JOptionPane.WARNING_MESSAGE);
        }
        
        try 
        {
            int num;
            PreparedStatement stmt = con.prepareStatement("UPDATE mytable SET name=?,age=?,gender=?,subject=? WHERE ID=?",Statement.RETURN_GENERATED_KEYS);
            stmt.setString(1, insertNameField.getText());
            stmt.setInt(2, parseInt(insertAgeCombo.getSelectedItem().toString()));
            stmt.setString(3, (insertMaleButton.isSelected()) ? "Male" : "Female");
            stmt.setString(4, insertSubjectField.getText());
            stmt.setInt(5,id);
            num = stmt.executeUpdate();
            JOptionPane.showMessageDialog(null, num + " rows altered!", "Message",JOptionPane.PLAIN_MESSAGE);
            stmt.close();
            (obj3).setVisible(false);
            (obj).setVisible(true);
        } 
        catch (SQLException ex) 
        {
            Logger.getLogger(InsertFrame.class.getName()).log(Level.SEVERE, null, ex);
        }
    }//GEN-LAST:event_jButton3ActionPerformed

    /**
     * @param args the command line arguments
     */
   /* public static void main(String args[]) {

        java.awt.EventQueue.invokeLater(new Runnable() {
            public void run() {
                new UpdateFrame().setVisible(true);
            }
        });
    }*/

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox insertAgeCombo;
    private javax.swing.ButtonGroup insertAgeGroup;
    private javax.swing.JRadioButton insertFemaleButton;
    private javax.swing.JTextField insertIDField;
    private javax.swing.JRadioButton insertMaleButton;
    private javax.swing.JTextField insertNameField;
    private javax.swing.JButton insertSearchButton;
    private javax.swing.JTextField insertSubjectField;
    private javax.swing.JButton jButton2;
    private javax.swing.JButton jButton3;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabel7;
    // End of variables declaration//GEN-END:variables
}
