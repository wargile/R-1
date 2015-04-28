# Demonstrating global variable handling
# --------------------------------------

#my.global <- 0 # only needed here if initGlobal() not called

initGlobal <- function() {
  my.global <<- 5 # Note <<- syntax!
}

updateGlobal <- function(val) {
  my.global <<- my.global * val # Note <<- syntax!
}

getGlobal <- function() {
  return (my.global)
}

initGlobal()
updateGlobal(12)
updateGlobal(-15)
getGlobal()
my.global
