
################################################################################################################################################
  ########################################################## Tower Of Hanoi ##################################################################
################################################################################################################################################

#" TYPE : USED TO DISTINGUISH BETWEEN 3 DISKS AND 6 DISKS PROBLEM  "
type <- as.integer(readline(prompt = "Enter number of rings and poles : "))

#" RUN : USED TO SPECIFY ITERATION FOR 3 DISKS AND 6 DISKS PROBLEM i.e. 16 AND 256  "
run  <- as.integer(readline(prompt = "Enter number of iterations : "))

#" TOWEROFHANOI : PLAYGROUND FOR TOWER OF HANOI GAME "
towerofhanoi <- matrix ( ncol = type, nrow = type)

#" VECTOR1 : USED TO INITIALIZE TOWER OF HANOI TOWERS "
vector1 <- numeric(type)


for(i in  type:1){							 								
	vector1[i] <- i																
}								 
for(i in 1:type){						
									
	for( j in 1:type){							
									
		if ( j == 1 )							
		{							
			towerofhanoi[i,j] <- vector1[i]				
		}								
		else								
		{								
			towerofhanoi[i,j] <- 0				
		}						
	}							
}							


#" CENTEROFMASS() : IS USED TO CALCULATE CENTER  OF MASS OF TOWER OF HANOI AFTER EACH DISK SHIFT "

centerofmass <- function(){
	
	
	sum1 <- rowSums(towerofhanoi)
	for(i in 1:type)
	{
		sum1[i] = (i-1)*sum1[i]		
	}
	
	
	return (sum(sum1)/((type*(type+1))/2))	
}

#" ROWSUM() : IS USED TO CALCULATE SUM OF A PERTICULAR ROW OF THE MATRIX ( TOWER OF HANOI ) "

rowSum <- function(tower)
{
	sumrow <- 0
	for(i in 1:type)
	{
		sumrow <- sumrow + towerofhanoi[tower,i]
	}
	sumrow
}

vacantIndex <- function ( tower )
{
	index <- 0
	for(i in 1:type)
	{
		if (towerofhanoi[tower,i] == 0)
		{
			index <- as.numeric(i)			
			return (index)
		
		}
		else if( i == type )
		{
			index <- as.numeric(i)+1
			return (index)
		}
		

	}

}

#" SHIFTDISK() : IS USED TO SHIFT DISKS FROM SOURCE TOWER TO TARGET TOWER WITH THE CONTRAINTS GIVEN IN THE QUESTION "

shiftDisk <- function(tower1)
{
	indexFrom <- 0
	indexTo <- 0
	if( tower1 == 1 )
	{
		
		indexFrom <- vacantIndex(tower1)-1
		indexTo <- vacantIndex(tower1+1)
		checkIndex <- 1		
		if(indexTo > 1)
		{		
			checkIndex <- indexTo - 1
		}
		if ((towerofhanoi[tower1+1,checkIndex] > towerofhanoi[tower1,indexFrom])|(towerofhanoi[tower1+1,checkIndex] == 0))
		{
			temp <- towerofhanoi[tower1+1,indexTo]
			towerofhanoi[tower1+1,indexTo] <- towerofhanoi[tower1,indexFrom]
			towerofhanoi[tower1,indexFrom] <- temp
		}
		
	}
	else if( tower1 == as.integer(type) )
	{	
		
		indexFrom <- vacantIndex(tower1)-1
		indexTo <- vacantIndex(tower1-1)
		checkIndex <- 1		

		if(indexTo > 1)
		{		
			checkIndex <- indexTo - 1
		}
				
		if ((towerofhanoi[tower1-1,checkIndex] > towerofhanoi[tower1,indexFrom])|(towerofhanoi[tower1-1,checkIndex] == 0))
		{
			temp <- towerofhanoi[tower1-1,indexTo]
			towerofhanoi[tower1-1,indexTo] <- towerofhanoi[tower1,indexFrom]
			towerofhanoi[tower1,indexFrom] <- temp
		}
	
	}
	else {
		random = sample(1:2,1,replace =T)
		if ( random == 2 )
		{	
			
			indexFrom <- vacantIndex(tower1)-1
			indexTo <- vacantIndex(tower1+1)
			checkIndex <- 1		

			if(indexTo > 1)
			{		
				checkIndex <- indexTo - 1
			}
					
			if ((towerofhanoi[tower1+1,checkIndex] > towerofhanoi[tower1,indexFrom])|(towerofhanoi[tower1+1,checkIndex] == 0))
			{
				temp <- towerofhanoi[tower1+1,indexTo]
				towerofhanoi[tower1+1,indexTo] <- towerofhanoi[tower1,indexFrom]
				towerofhanoi[tower1,indexFrom] <- temp
			}		
		}
		else 
		{
		
			indexFrom <- vacantIndex(tower1)-1
			indexTo <- vacantIndex(tower1-1)
			checkIndex <- 1		

			if(indexTo > 1)
			{		
				checkIndex <- indexTo - 1
			}
			if ((towerofhanoi[tower1-1,checkIndex] > towerofhanoi[tower1,indexFrom])|(towerofhanoi[tower1-1,checkIndex] == 0))
			{
				temp <- towerofhanoi[tower1-1,indexTo]
				towerofhanoi[tower1-1,indexTo] <- towerofhanoi[tower1,indexFrom]
				towerofhanoi[tower1,indexFrom] <- temp
			}
		}
	}
	towerofhanoi	
}



centerofmass1 <- numeric(run)

#"MEDIAN1() : USED TO CALCULATE MEDIAN OF CENTER OF MASS FOR EVERY ITERATION OF TOWER OF HANOI "
mean1 <- function(  ){

	tower <- 0
	for(i in 1:run )
	{
		repeat{
			tower =	sample(1:sample(1:type,1,replace = F),1,replace = F)
			if(rowSum(tower) != 0){
				break 
			}
		}
	  #print(tower)
		towerofhanoi <<- shiftDisk(tower)
		print(towerofhanoi)
  	centerofmass1[i] <<- centerofmass()
		#centerofmass1[i]
	}
	print( "Mean = " )
	mean(centerofmass1)
}

#"SDEVIATION() : USED TO CALCULATE STANDERD DEVIATION OF CENTER OF MASS FOR EVERY ITERATION OF TOWER OF HANOI "

sDeviation <- function( ){

  tower <- 0
  for(i in 1:run )
  {
    repeat{
      tower =	sample(1:sample(1:type,1,replace = F),1,replace = F)
      if(rowSum(tower) != 0){
        break 
      }
    }
    #print(tower)
    towerofhanoi <<- shiftDisk(tower)
    print(towerofhanoi)
    centerofmass1[i] <<- centerofmass()
    #centerofmass1[i]
  }
  
  print( "Standerd Deviation = " )
  sd(centerofmass1,mean(centerofmass1))
}


