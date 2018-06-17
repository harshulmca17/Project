

################################################################################################################################################
  ########################################################## Tower Of Hanoi ##################################################################
################################################################################################################################################


------------------------------------------------------------------------------------------------------------------------------------------------

							DESCRIPTION OF VARIABLES

					-------------------------------------------------------------- 
		
	* TYPE : IS A VARIABLE USED TO DISTINGUISH BETWEEN PROBLEM OF 3 AND 6 DISKS.

	* RUN : IS A VARIABLE USED TO HOLD THE NUMBER OF ITERATION GOING TO PERFORM FOR DISK 3 i.e. 16 AND FOR DISKS 6 i.e. 256.
		
	* CENTEROFMASS1 : IS A VECTOR OF NUMERIC TYPE OF SIZE ( RUN = 16 ( DISKS = 3) OR RUN = 256 (DISKS = 6) ).

	* TOWEROFHANOI  : IS A MATRIX OF NUMERIC TYPE OF SIZE ( TYPE x TYPE ) AND REPRESENTING THE TOWER OF HANOI PLAY GROUND FOR THE PROBLEM
		
-----------------------------------------------------------------------------------------------------------------------------------------------
			
							DESCRIPTION OF FUNCTIONS 						

					---------------------------------------------------------------

	
	* CENTEROFMASS() :
				OBJECTIVE :  	TO CALCULATE CENTER OF MASS OF TOWER OF HANOI FOR A PERTICULAR ITERATION
				INPUT 	  :  	NA	
				OUTPUT    :  	CENTER OF MASS OF PRESENT CONDITION OF TOWER OF HANOI					 
				
				USE	  :  	calling this function after shiftDisk( tower ) to calculate center of mass of tower of hanoi 
						after each iteration

	* MEAN1() :
				OBJECTIVE :  	TO CALCULATE MEAN OF ALL CENTER OF MASSES OF TOWER OF HANOI
				INPUT 	  :  	NA	
				OUTPUT    :  	MEAN OF CENTER OF MASSES OF TOWER OF HANOI	

				use 	  :	calling this function to calculate mean of all center of masses calculated after each iteration

	* ROWSUM() :
				OBJECTIVE :  	TO CALCULATE SUM OF THE ROW MENTIONED BY TOWER VARIABLE
				INPUT 	  :  	TOWER ( CALCULATED RANDOMLY ) 	
				OUTPUT    :  	SUM OF ROW PROVIDED BY TOWER VARIABLE 	

				use 	  : 	this function is calculating the sum of all the elemnts of given row

	* SDEVIATION() :
				OBJECTIVE :  	TO CALCULATE STANDARD DEVIATION OF CENTER OF MASS OF TOWER OF HANOI FOR ALL ITERATIONS
				INPUT 	  :  	NA	
				OUTPUT    :  	STANDARD DEVIATION OF CENTER OF MASSES OF ALL ITERATION

				use 	  :	calling this function to calculate standard deviation of all center of masses along with the 							calculated mean  after each iteration

	* SHIFTDISK() :
				OBJECTIVE :  	TO SHIFT DISK FROM TOWER VARIABLE ( CALCULATED RANDOMLY ) TO (TOWER-1) OR (TOWER+1) STACK
				INPUT 	  :  	TOWER ( HOLDING THE RANDOM STACK FROM WHICH DISK HAS TO BE MOVE TO ANY OTHER STACK
				OUTPUT    :  	TOWER OF HANOI MATRIX AFTER DISK SHIFTING
	
				use 	  :	this function is going to shift disk from stack tower( calculated randomly ) to 						tower' ( calculated randomly )

	* VACANTINDEX() :
				OBJECTIVE :  	TO CACULATE INDEX OF CELL HOLDING 0 IN IT
				INPUT 	  :  	TOWER ( HOLDING THE RANDOM STACK WHOSE SUM HAS TO BE CACULATED )	
				OUTPUT    :  	INDEX OF CELL HOLDING 0 IN IT	
				
				use 	  :	this function is used to return index of the cell from where we have to pick up the disk and to 						index where we have to drop it



-----------------------------------------------------------------------------------------------------------------------------------------------


								GROUP MEMBERS : 
					-----------------------------------------------------------------------		
							NAME :		Roll No.:	Class:
	
							Sheenam Yadav	 34	 	MCA 1st year
							
							Harshul Kumar 	 14	 	MCA 1st year
