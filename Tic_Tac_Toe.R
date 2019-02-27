# Making game board
triples <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))
winner<-FALSE
state<-as.character(1:9)

# Displaying the Board
display<-function(state){
  cat(" ",state[1],"|",state[2],"|",state[3],"\n")
  cat(" ---+---+---","\n")
  cat(" ",state[4],"|",state[5],"|",state[6],"\n")
  cat(" ---+---+---","\n")
  cat(" ",state[7],"|",state[8],"|",state[9],"\n")
}

# Declaring X and O as the player Id's
update<-function(state, who, pos){
  if(who==1){state[pos]<-"x"}
  else if(who==2){state[pos]<-"o"}
  return(state)
}

# Checking who wins when all 3 rows od X or o meet diagonally or column wise
check_winner<-function(state){
  for(i in 1:length(triples)){
    if(sum(triples[[i]] %in% which(state=="x"))>=3){cat("x wins, type play() to play again","\n");winner=TRUE;break}
  }
  for(i in 1:length(triples)){
    if(sum(triples[[i]] %in% which(state=="o"))>=3){cat("o wins, type play() to play again","\n");winner=TRUE;break}
  }
  if(sum(state==as.character(1:9))==0 && winner==FALSE){cat("game draw,type play() to play again.");winner=TRUE}
  return(winner)
}

# checking if user cheat typing multiple times
check_illegal<-function(state){
  illegal<-TRUE
  while(illegal){
    pos<-as.double(readline("which position to play(type 1 to 9):"))
    if(pos!=1&&pos!=2&&pos!=3&&pos!=4&&pos!=5&&pos!=6&&pos!=7&&pos!=8&&pos!=9){
      cat("please type a number from 1 to 9","\n")
    }else{
      if(state[pos] != "x" && state[pos] != "o"){
        state<-update(state,pos)
        illegal=FALSE
      }else if(state[pos] == "x" || state[pos] == "o"){
        cat("you already played there, try again","\n")
      }
    }
  }
  return(pos)
}


# Computer Logics for positions regarding tuples
computer_turn<-function(state){
  if(sum(state=="x") > sum(state=="o")){#human goes first playing x and computer second playing o
    before<-state
    for(i in 1:length(triples)){
      if(sum(triples[[i]] %in% which(state=="o"))==2 && 
         any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9)) && any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))){#winning
        state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]]<-"o"
      }
      if(identical(state,before)==FALSE)break
    }
    if(identical(state,before)==TRUE){
      for(i in 1:length(triples)){
        if(sum(triples[[i]] %in% which(state=="x"))==2 && 
           any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9))){#blocking
          state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]]<-"o"
        }
        if(identical(state,before)==FALSE)break
      }
    }
    if(identical(state,before)==TRUE){state[sample(which(state == as.character(1:9)),1)]<-"o"}
  }else if(sum(state=="o") >= sum(state=="x") || sum(state=="o")==0){#computer goes first playing x and human second playing o
    before<-state
    for(i in 1:length(triples)){
      if(sum(triples[[i]] %in% which(state=="x"))==2 &&
         any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9))){#winning
        state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]]<-"x"
      }
      if(identical(state,before)==FALSE)break
    }
    
    
    if(identical(state,before)==TRUE){
      for(i in 1:length(triples)){
        if(sum(triples[[i]] %in% which(state=="o"))==2 && 
           any(state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]] %in% as.character(1:9))&& any(state[triples[[i]][which((triples[[i]] %in% which(state=="x"))==FALSE)]] %in% as.character(1:9))){#blocking
          state[triples[[i]][which((triples[[i]] %in% which(state=="o"))==FALSE)]]<-"x"
        }
        if(identical(state,before)==FALSE)break
      }
    }
    if(identical(state,before)==TRUE){state[sample(which(state == as.character(1:9)),1)]<-"x"}
  }
  return(state)
} 


# human verse computer
human_verse_computer<-function(state,who){
  while(winner == FALSE){
    if(who==1){
      pos<-check_illegal(state)
      state<-update(state, who, pos) 
      display(state) 
      winner<-check_winner(state)
      if(winner) break
      state<-computer_turn(state)
      cat("computer plays:","\n")
      display(state)
      winner<-check_winner(state)
    }
    else if(who==2){
      state<-computer_turn(state)
      cat("computer plays:","\n")
      display(state) 
      winner<-check_winner(state)
      if(winner) break
      pos<-check_illegal(state)
      state<-update(state, who, pos)
      display(state)
      winner<-check_winner(state)
    }
  }
}

# human verse human
human_verse_human<-function(state,who){
  while(winner == FALSE){
    pos<-check_illegal(state)
    if(who==1){# x's turn
      state<-update(state, who, pos) 
      display(state) 
      winner<-check_winner(state) 
      who<-2 # will this 2 lead to "else if" loop?
    }
    else if(who==2){# o's turn
      state<-update(state, who, pos) 
      display(state)
      winner<-check_winner(state) 
      who<-1
    }
  }
}

# Type "play()" to begin
play <- function(){
  computer_or_not<-readline("how many players?(type 1 to play against computer):")
  
  if(computer_or_not==1){
    who<-readline("you want to go first(type 1) or second(type 2):");cat("x goes first","\n")
    display(state)
    human_verse_computer(state,who)
  }
  
  else if(computer_or_not==2){
    who<-1;cat("x goes first","\n")
    display(state)
    human_verse_human(state,who)
  }
}