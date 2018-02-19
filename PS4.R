# Kalen Davison
# Professor Montgomery
# PS4
# Due 2/22/18

### fixing the "starter function" ###
myFunction <- function(car, choice){
  if (car==choice){
    print(TRUE)
  } else 
      print(FALSE)
}

myFunction(3, 3) #TRUE
myFunction(1, 2) #FALSE

# Should return a TRUE if these samples are equal and
# a false if they are not

### Part 1 ###

#Create a new S4 class door

setClass(Class = "door", #creates door class in S4, numeric data.
         representation = representation(
           chosenDoor = "numeric",
           carDoor = "numeric",
           switch = "logical"
         ),
         prototype = prototype(
           chosenDoor = c(),
           carDoor = c(),
           switch = c()
         )
)

# Construction function that creates a new object of class door
setMethod("initialize", "door", function(.Object, ...) {
         value = callNextMethod()
         validObject(value)
         return(value)
}
)


# a validation function that checks whether the values stored in the
# slots are appropriately structured.

setValidity("door", function(object){
  check_value1 = object@chosenDoor == 1 | object@chosenDoor == 2 | object@chosenDoor == 3
  check_length1 = (length(object@chosenDoor == 1))
  check_value2 = object@carDoor == 1 | object@carDoor == 2 | object@carDoor == 3
  check_length2 = (length(object@carDoor == 1))
  check_value3 = object@switch == TRUE | object@switch == FALSE
  check_length3 = (length(object@switch == 1))
  
  if (!check_value1 | !check_length1){return("object@chosenDoor is not valid, must be 1, 2, or 3")}
  if (!check_value2 | !check_length2){return("object@carDoor is not valid, must be 1, 2, or 3")}
  if (!check_value3 | !check_length3){return("object@switch must be TRUE or FALSE")}
}
)


### Part 2 ###

setGeneric("PlayGame", #creates a generic function in S4 that takes objects of class "door" as arguments
           function(object="door") {
             standardGeneric("PlayGame")
           } )

setMethod("PlayGame", "door", #creates a method for the above generic function for class "door" 
          function(object){
           object@carDoor = sample (1:3, 1)
           firstDoor = sample(1:3, 1)
           if (object@switch == FALSE) {
             firstDoor = object@chosenDoor
           }
           if (object@switch == TRUE)  {  
             control = FALSE
             while(control == FALSE){ #while loop that keeps randomly picking a door until it satisfies the criteria that it's not the same as the car door or the first door chosen by the contestant.
               object@chosenDoor = sample(1:3, 1)
               if (object@chosenDoor != object@carDoor & object@chosenDoor != firstDoor){
                 control = TRUE
               }
             } #below is read only if the chosen door is not the same as the car door or first door chosen
               if (object@chosenDoor == 1){ 
               choices = c(2, 3)
             } else if (object@chosenDoor == 2){
                 choices = c(1, 3)
             } else if (object@chosenDoor == 3){
                choices = c(1, 2) # choosing only between the doors still available
             } 
             object@chosenDoor = sample(choices, 1) #choose randomly between one of the two doors available
           } 
           winner = (object@carDoor == object@chosenDoor) #returns TRUE if contestant chooses the car door and FALSE if they don't ultimately.
           return(winner)
          } 
           )

debug(PlayGame)
PlayGame(switch)
undebug(PlayGame)
traceback()

### Part 3: Simulation ###

#1) No Switch Strategy
PlayNoSwitch = function(i){  #Not switch strategy function to be inputted into sapply function
  test = new("door", chosenDoor = 2, carDoor = 1, switch = FALSE)
  out = PlayGame(test)
  return(out)
}

NoSwitch = sapply(c(1:1000), PlayNoSwitch)
NoSwitchTable = table(NoSwitch)
NoSwitchTable
PercentWonNoSwitch = NoSwitchTable[2]/10
paste("Player that did not switch won", PercentWonNoSwitch, "% of the time.") # around 33.3% win rate

#3) Switch Strategy

PlaySwitch = function(i){ #Switch strategy function to be input into sapply function
  test = new("door", chosenDoor = 2, carDoor = 1, switch = TRUE)
  out = PlayGame(test)
  return(out)
}

Switch = sapply(c(1:1000), PlaySwitch)
SwitchTable = table(Switch)
SwitchTable
PercentWonSwitch = SwitchTable[2]/10
paste("Player that switched won", PercentWonSwitch, "% of the time.") # around 50% win rate


#After comparing the percentages from both strategies, it is clear that switching is a better strategy.

