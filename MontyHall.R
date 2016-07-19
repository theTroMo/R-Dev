##Troy Morse 4/8/2016
##Data Science 350 - Methonds for data analysis
##assignemt 2 - Monty Hall Problem

## Standard assumptions
# 1.The host must always open a door that was not picked by the contestant (Mueser and Granberg 1999).
# 2.The host must always open a door to reveal a goat and never the car.
# 3.The host must always offer the chance to switch between the originally chosen door and the remaining closed door.


Distribute_Prizes_Behind_Doors = function(GameDoors, bigSC){
  BSC_Door = sample(c("Door1","Door2","Door3"),1)
  GameDoors[BSC_Door] = bigSC

  return(GameDoors)
}

Get_Players_First_Pick = function(Player){  #Player, GameDoors){
  Player = sample(c("Door1","Door2","Door3"),1)
  return(Player)
}

Reveal_Unchosen_Door = function(GameDoors, bigSC, Player){
    pool = c("Door1","Door2","Door3")
    pool <- pool[which(pool!=Player)]
    UC_Doors <- sample(pool,1)
    if (GameDoors[UC_Doors] == bigSC) {
      pool<-pool[which (pool != UC_Doors)]
    }
    return(UC_Doors)
}

##runLoop is the driver function to simulate Let's make a Deal
##
runLoop = function(TakeNewDoor,bigSC,WinLose){
  threeLG <- "Three legged Goat"
  bigSC <- "Big Stack of Cash"
  GameDoors = list(Door1=c(threeLG), Door2=c(threeLG), Door3=c(threeLG))
  GameDoors = Distribute_Prizes_Behind_Doors(GameDoors,bigSC)

  Player = "Player"
  Player = Get_Players_First_Pick(Player)
  RevealedDoor = Reveal_Unchosen_Door(GameDoors, bigSC, Player)
  WinLose = "NULL"
  
  if (TakeNewDoor == TRUE) {
    if (GameDoors[RevealedDoor] == bigSC) {
      WinLose = 1
    } else {WinLose = 0}
  }
  
  if (TakeNewDoor == FALSE) {
    if (GameDoors[Player] == bigSC) {
      WinLose = 1
    } else {WinLose = 0}
  }  
#  df <- data.frame(GameDoors[Player],GameDoors[RevealedDoor], stringsAsFactors = FALSE)
#  df <- data.frame(df$Player=GameDoors[Player],df$RevealedDoor=GameDoors[RevealedDoor], stringsAsFactors = FALSE)
  df = data.frame(Player,RevealedDoor, WinLose,stringsAsFactors = FALSE)
  df$Player = GameDoors[Player]
  df$RevealedDoor = GameDoors[RevealedDoor]
  
  return(df)  
}

test_runLoop = function(){
  test_data = data.frame(testPlayer,testRevealDoor,testWinLose,stringsAsFactors = FALSE)
  test_output = runLoop(FALSE,"Big Stack of Cash",NULL)
  #expected_output = c(1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9)
  stopifnot(test_output == test_data)
}

if (interactive()){

    # Set logging information
  library(logging)
  basicConfig()
  addHandler(writeToFile, logger="MH_logger", file="file_log.log")
  
  # Set working director
  loginfo("Setting wd", logger="MH_logger")
  setwd('C:/Users/troy_/OneDrive/@UW_dataSci/UW_spring_2016/w2')
    loginfo(paste('##Troy Morse 4/8/2016'), logger="MH_logger")
    loginfo(paste("##Data Science 350 - Methonds for data analysis"), logger="MH_logger")
    loginfo(paste("##assignemt 2 - Monty Hall Problem"), logger="MH_logger")
  
  # Run unit test:
  source('MH_UnitTests.R')
  
  #Setting up game  - creating doors & distributing prizes  
    logStr = paste("Setting up Game Doors with default prizes")
    loginfo(paste(logStr, logger="MG_logger"))
    threeLG <- "Three legged Goat"
    bigSC <- "Big Stack of Cash"
    GameDoors = list(Door1=c(threeLG), Door2=c(threeLG), Door3=c(threeLG))
      
  GameDoors = Distribute_Prizes_Behind_Doors(GameDoors,bigSC)
    logStr = paste(GameDoors["Door1"],GameDoors["Door2"],GameDoors["Door3"])
    loginfo(paste("The prize is are behind the doors as follow: "), logger="MH_logger")
    loginfo(paste(logStr, logger="MH_logger"))

  #init host & play
  Player = "Player"
  Host = "Host"
  
  #Players first pick
  Player = Get_Players_First_Pick(Player)
    logStr = paste(Player)
    loginfo(paste("The players first pick is", Player), logger="MH_logger")
  
  #Host reveals other Door
  RevealedDoor = Reveal_Unchosen_Door(GameDoors, bigSC, Player)
  logStr = paste(RevealedDoor)
  loginfo(paste("The host revealed ", RevealedDoor), logger="MH_logger")

  choice = TRUE    #choice determines wether the player stands with the first pick or revealed door 
  WinLose = "NULL"  #WinLose is the result of the game
  numOfObs = 500
  df <- runLoop(choice,bigSC,WinLose)
  for(i in 1:numOfObs) df <- rbind(df,runLoop(choice,bigSC,WinLose))
  
  WL_sum = sum(df$WinLose)
  winProb = (WL_sum/numOfObs)
  varWL = var(df$WinLose)
  outFile = paste(choice,"outFile.txt")
  
  addHandler(writeToFile, logger="out_logger", file=outFile)
    loginfo(paste('##Troy Morse 4/8/2016'), logger="out_logger")
    loginfo(paste("##Data Science 350 - Methonds for data analysis"), logger="out_logger")
    loginfo(paste("##assignemt 2 - Monty Hall Problem"), logger="out_logger")
    
    loginfo(paste("#######################################"), logger = "out_logger")
    loginfo(paste("#######################################"), logger = "out_logger")
    loginfo(paste("The players decision to pick a new door is ", choice), logger="out_logger")
    loginfo(paste("The final probability for this pick is: ", winProb), logger = "out_logger")
    loginfo(paste("The final variance for this pick is: ", varWL), logger = "out_logger")
    loginfo(paste("#######################################"), logger = "out_logger")
    loginfo(paste("#######################################"), logger = "out_logger")

  #loginfo(paste(df),logger = "out_logger")
}
