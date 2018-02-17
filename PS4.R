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

#1)

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
  
  if (!check_value1 | !check_length1){return("object@chosenDoor is not valid")}
  if (!check_value2 | !check_length2){return("object@carDoor is not valid")}
  if (!check_value3 | !check_length3){return("object@switch is not valid")}
}
)


#2)


