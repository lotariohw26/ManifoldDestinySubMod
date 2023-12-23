# plumber.R

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @get /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}


#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum2
function(a, b) {
  as.numeric(a) + as.numeric(b)
}



library(plumber)
# Path to API definition
apir <- plumber::plumb("plumber.R") 
plumber::pr_run(apir,host = "0.0.0.0", port = 2000)
