

################################################################################################################################################
  ########################################################## Tower Of Hanoi ##################################################################
################################################################################################################################################


------------------------------------------------------------------------------------------------------------------------------------------------

							DESCRIPTION OF VARIABLES

					-------------------------------------------------------------- 
		
	* DATA1 : read the data from table "LoanStats3c.csv" using read.csv 


	* DATA2 : read the data from table "LoanStats3d.csv" using read.csv

        * DATA3 : combining the contents of two tables using rbind
Q1.
	* Loan_Amount : used to fetch the column loan_amnt from DATA3

        * Corrected_Loan_Amount : Stores all the data of Loan_Amount in the form of a column except null values
      
        * MedianLoanAmount : contins the median of all the tuples of Corrected_Loan_Amount
Q2.
        * purpose : used to fetch the columns contents of column 'purpose' from DATA3

        * most_common : stores the number of times the highest occured purpose has occured (here the 'purpose' "debt_consolidation" has occured 
                        the most) 
        * Length_Purpose : stores the length of the column 'purpose'

        * fraction : stores the result generated when we divide most_common by Length_Purpose
Q3.
        * Purposes : used to fetch the columns contents of column 'purpose' from DATA3

        * Rate : used to fetch the columns contents of column 'int_rate' from DATA3
   
        * Rate1 : stores the converted value of the 'int_rate' or 'Rate'

        * mean_car : stores the mean where 'purpose' is "car"
 
        * mean_creditcard : stores the mean where 'purpose' is "credit_card" 

        * mean_debit : stores the mean where 'purpose' is "debit_conslidation" 

        * mean_home : stores the mean where 'purpose' is "home_improvement" 

        * mean_house : stores the mean where 'purpose' is "house" 

        * mean_mp : stores the mean where 'purpose' is "major_purchase" 

        * mean_med : stores the mean where 'purpose' is "medical"
 
        * mean_mov : stores the mean where 'purpose' is "moving" ....and similarly for other purposes
Q4.
        * count : list all the distinct values under 'Term1' in "DATA1"
       
        * calc1 : contains the number of occurences of the element at position 2 (in this case it is 36 months)
Q5.
        * Issued_date1 : converts the issue date into format 'date month'
        
        * S.D. : contains all the required ratios except the null values
Q6.
        * Paid_loans : contains data from DATA3 where loan_status is "Fully_paid"			
-----------------------------------------------------------------------------------------------------------------------------------------------


								GROUP MEMBERS : 
					-----------------------------------------------------------------------		
							NAME :		Roll No.:	Class:
	
							Sheenam Yadav	 34	 	MCA 1st year
							
							Harshul Kumar 	 14	 	MCA 1st year
