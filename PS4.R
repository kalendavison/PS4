# Kalen Davison
# PS4
# Due 2/22/18

myFunction <- function(car, choice){
  if (car==choice){
    print(TRUE)
  } else 
      print(FALSE)
}

myFunction(3, 3)

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
           browser()
           if (object@switch == TRUE)  {  
             control = FALSE
             while(control == FALSE){
               object@chosenDoor = sample(1:3, 1)
               if (object@chosenDoor != object@carDoor & object@chosenDoor != firstDoor){
                 control = TRUE
               }
             }
             if (object@chosenDoor == 1){
               choices = c(2, 3)
             } else if (object@chosenDoor == 2){
               choices = c(1, 3)
             } else if (object@chosenDoor == 3){
               choices = c(1, 2)
             } 
             object@chosenDoor = sample(choices, 1)
           } 
           winner = (object@carDoor == object@chosenDoor)
           return(winner)
          } 
           )


### Part 3: Simulation ###

switch = new("door", chosenDoor = 1, carDoor = 2, switch = TRUE)
PlayGame(switch)

PlaySwitch = function(i){
  test = new("door", chosenDoor = 2, carDoor = 1, switch = TRUE)
  out = PlayGame(test)
  return(out)
}

Switch = sapply(c(1:1000), PlaySwitch)
SwitchTable = table(Switch)
SwitchTable
PercentWonSwitch = SwitchTable[2]/10
paste("Player that switched won", PercentWonSwitch, "% of the time.")



noSwitch = new("door", chosenDoor = 3, carDoor = 2, switch = FALSE)
PlayGame(noswitch)

PlayNoSwitch = function(i){
  test = new("door", chosenDoor = 2, carDoor = 1, switch = FALSE)
  out = PlayGame(test)
  return(out)
}

NoSwitch = sapply(c(1:1000), PlayNoSwitch)
NoSwitchTable = table(NoSwitch)
NoSwitchTable
PercentWonNoSwitch = NoSwitchTable[2]/10
paste("Player that did not switch won", PercentWonNoSwitch, "% of the time.")

#Switch wins more.
