#########################################################################################################################################################
################################################################      CARD  21        ###################################################################
################################################################        Game          ###################################################################
################################################################ MCA-103 Assignment 2 ###################################################################
#########################################################################################################################################################



#########################################################################################################################################################
## DECLERATION AND INITIALIZATION

playingCard <- as.numeric()
top <- 1
temptop <-top
sumUser <- 0
sumComputer <- 0
reply <- "HIT"

#########################################################################################################################################################
## INIT() FUNCTION AS REQUIRED

init <-function(){
  
  for(i in 1:52){
    
    if(i<14)
    {
      if ( i <= 10 )
      {
        playingCard <<- c(playingCard,spade=i)
      }
      else{
        playingCard <<- c(playingCard,spade=10)
      }
      
    }
    else if(i>13&&i<27)
    {
      if ( (i-13) <= 10 )
      {
        playingCard <<- c(playingCard,heart=i-13)
      }
      else
      {
        playingCard <<- c(playingCard,heart=10)
      }
    }
    else if(i>26&&i<40)
    {
      if ( (i-26) <= 10 )
      {
        playingCard <<- c(playingCard,diamond=i-26)
      }
      else
      {
        playingCard <<- c(playingCard,diamond=10)
      }
    }
    else if(i>39&&i<53)
    {
      if ( (i-39) <= 10 )
      {
        playingCard <<- c(playingCard,club=i-39)
      }
      else{
        playingCard <<- c(playingCard,club=10)
      }
    }
  }
}
#########################################################################################################################################################
## PRINTNAME() FUNCTION USED TO PRIND CARD INDEX WITH ITS SUIT

printname <- function(index){
  
  print(paste0(playingCard[index]," of ",names(playingCard[index])))
  
}


#########################################################################################################################################################
## PROBABILITY() AS REQUIRED ( OPTIONAL BY QUESTION ) USED TO CALCULATE PROBABILITY OF CARDS WHEN WE CHOOSE THEM WE GET 21

probability <- function(j){
  
  counter <- 0
  
  for (i in temptop:52){
    
    if(j==1){
        if(i <= (21-sumUser))
          {
            counter <- counter+1
          }
    }
    else if(j==2){
      if(i <= (21-computerUser))
      {
        counter <- counter+1
      }
    }
  }
  return(counter/(53-temptop))
  
}


#########################################################################################################################################################
## SHUFFLE() FUNCTION AS REQUIRED IS USED TO SHUFFLE DECK 

shuffle <- function(){
  print(paste0("Shuffling Cards"))
  for( i in 1:51)
  {
    index <- sample(i:50, 1)
    temp <- playingCard[i]
    playingCard[i] <<- playingCard[index]
    playingCard[index] <<- temp
    
    temp <- names(playingCard[i]) 
    names(playingCard)[i]<<-paste(names(playingCard[index]))
    names(playingCard)[index]<<-paste(temp)
  }
}


#########################################################################################################################################################
## DEAL() FUNCTION AS REQUIRED IS USED TO RETURN CARD ON TOP AND THEN POINT TO THE NEXT CARD

deal <- function(){
  
  temptop <<- top
  top <<- top+1
  return(temptop)
}


#########################################################################################################################################################
## USERTURN() IS USED TO CHOOSE NEXT VALID CARD FOR USER

userTurn <-function(){
  printname(deal())
  if(temptop == 1){
    if((sumUser+11)<21)
    {
      return(1)  
    }
    else{
      return(11)
    }
  }
  else{
    return(playingCard[temptop])
  }
}


#########################################################################################################################################################
## COMPUTERTURN() IS USED TO CHOOSE NEXT VALID CARD FOR COMPUTER

computerTurn <- function(){
  printname(deal())
  if(temptop == 1){
    if((sumUser+11)<21)
    {
      return(1)  
    }
    else{
      return(11)
    }
  }
  else{
    return(playingCard[temptop])
  }
}


#########################################################################################################################################################
## TWENTYONE() FUNCTION AS REQUIRED IS USED AS MAIN FUNCTION 

twentyOne <-function(){
  
    init()
  repeat{
    
    shuffle()
    
    print(paste0("******User's Turn*******"))
    
    sumUser <<- as.integer(playingCard[deal()])
    printname(temptop)
    sumUser <<- sumUser + as.integer(playingCard[deal()])
    printname(temptop)
    repeat{
      
      print(paste0("Probability Of Next Success is ",probability(1)))
      reply <<- readline(prompt = "HIT OR STAY ?")
      
      
          
      if(sumUser >= 21){
        
        break
      }
      if(reply == "STAY" | reply == "stay")
      {
        
        break
      }
      else
        {
          print(paste0("Your Score :  ",sumUser))
          
        sumUser <<- sumUser + as.integer(userTurn())
        
      }
      
    }
    print(paste0("******Computer's Turn*******"))
    if(sumUser <= 21){
      sumComputer <<- as.integer(playingCard[deal()])
      printname(temptop)
      sumComputer <<- sumComputer + as.integer(playingCard[deal()])
      printname(temptop)
      repeat{
        print(paste0("HIT OR STAY ?"))
        
        if(sumComputer >= 21 ){
          print(paste0(reply))
          
          break
        }
        else{
          
          if(sumComputer < 17)
          {
            reply <<- "HIT"
          }
          else{
            choice <- probability(2)
            if( choice > 0.1)
            {
              reply <<- "HIT"
            }
            else
            {
              reply <<- "STAY"
            }
            
          }
          print(paste0(reply))
          if( reply == "STAY")
          {
            break
          }
          print(paste0("Your Score :  ",sumUser))
          
          sumComputer <<- sumComputer + as.integer(computerTurn())
        }    
      }
      
    }
    else{
      print(paste0("YOU LOSE "))
      
    }
    
    print(paste0("Computer Score : ",sumComputer))
    
    
    if(sumUser == 21)
    {
      print(paste0("YOU WIN "))
      
    }
    else if(sumComputer == 21)
    {
      print(paste0("YOU LOSE. "))
      
    }
    else if(sumUser > sumComputer)
    {
      print(paste0("YOU WIN "))
    }
    else if(sumUser < sumComputer)
    {
      print(paste0("YOU LOSE. "))
    }
    else if(sumUser == sumComputer)
    {
      print(paste0("GAME DRAW. "))
    }
    
    reply <<- readline(prompt = "PLAY AGAIN (Y/N) ?")
    if(reply == "N")
    {
      break
    }
    else{
      top <<- 1
      temptop <<-top
      sumComputer <<- 0
      sumUser <<-0
    }
  }
  
}



#########################################################################################################################################################
###############################################################     END      ############################################################################
#########################################################################################################################################################


#########################################################################################################################################################
##                                            TEAM MEMBER NAME : ROLL NO. 
##                                         1. HARSHUL KUMAR    : 14
##                                         2. DIKSHA VERMA     : 11
##                                         3. Md. Shoaib Rayeen: 21   
#########################################################################################################################################################

#########################################################################################################################################################
## GAME START

twentyOne()

\\
