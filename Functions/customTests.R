test_func1 <- function() {
  try({
    func <- get('boring_function', globalenv())
    t1 <- identical(func(9), 9)
    t2 <- identical(func(4), 4)
    t3 <- identical(func(0), 0)
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_func2 <- function() {
  try({
    func <- get('my_mean', globalenv())
    t1 <- identical(func(9), mean(9))
    t2 <- identical(func(1:10), mean(1:10))
    t3 <- identical(func(c(-5, -2, 4, 10)), mean(c(-5, -2, 4, 10)))
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_func3 <- function() {
  try({
    func <- get('remainder', globalenv())
    t1 <- identical(func(9, 4), 9 %% 4)
    t2 <- identical(func(divisor = 5, num = 2), 2 %% 5)
    t3 <- identical(func(5), 5 %% 2)
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_func4 <- function() {
  try({
    func <- get('evaluate', globalenv())
    t1 <- identical(func(sum, c(2, 4, 7)), 13)
    t2 <- identical(func(median, c(9, 200, 100)), 100)
    t3 <- identical(func(floor, 12.1), 12)
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_func5 <- function() {
  try({
    func <- get('telegram', globalenv())
    t1 <- identical(func("Good", "morning"), "START Good morning STOP")
    t2 <- identical(func("hello", "there", "sir"), "START hello there sir STOP")
    t3 <- identical(func(), "START STOP")
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_func6 <- function() {
  try({
    func <- get('mad_libs', globalenv())
    t1 <- identical(func(place = "Baltimore", adjective = "smelly", noun = "Roger Peng statue"), "News from Baltimore today where smelly students took to the streets in protest of the new Roger Peng statue being installed on campus.")
    t2 <- identical(func(place = "Washington", adjective = "angry", noun = "Shake Shack"), "News from Washington today where angry students took to the streets in protest of the new Shake Shack being installed on campus.")
    ok <- all(t1, t2)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_func7 <- function() {
  try({
    func <- get('%p%', globalenv())
    t1 <- identical(func("Good", "job!"), "Good job!")
    t2 <- identical(func("one", func("two", "three")), "one two three")
    ok <- all(t1, t2)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_eval1 <- function(){
  try({
    e <- get("e", parent.frame())
    expr <- e$expr
    t1 <- identical(expr[[3]], 6)
    expr[[3]] <- 7
    t2 <- identical(eval(expr), 8)
    ok <- all(t1, t2)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_eval2 <- function(){
  try({
    e <- get("e", parent.frame())
    expr <- e$expr
    t1 <- identical(expr[[3]], quote(c(8, 4, 0)))
    t2 <- identical(expr[[1]], quote(evaluate))
    expr[[3]] <- c(5, 6)
    t3 <- identical(eval(expr), 5)
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

test_eval3 <- function(){
  try({
    e <- get("e", parent.frame())
    expr <- e$expr
    t1 <- identical(expr[[3]], quote(c(8, 4, 0)))
    t2 <- identical(expr[[1]], quote(evaluate))
    expr[[3]] <- c(5, 6)
    t3 <- identical(eval(expr), 6)
    ok <- all(t1, t2, t3)
  }, silent = TRUE)
  exists('ok') && isTRUE(ok)
}

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Get the value which a user either entered directly or was computed
# by the command he or she entered.
getVal <- function(){
  getState()$val
}

# Get the last expression which the user entered at the R console.
getExpr <- function(){
  getState()$expr
}

#######################################################################################
# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

# take care of subbmition
submition <- function() {
  
  # getting envirament
  #e <- get("e", parent.frame())
  #EE <<- e
  #print (EE)
  
  # is answar is No - than it is OK - doing nothing
  if(e$val == "No") return(TRUE)
  
  if (e$skipped){
    msg = sprintf("It seems that you have skipped some questions, namely %d.\nIt is highly recommended to submit result only then you finished it completely.", e$skips)
    message(msg)
    message("Do you want to proceed anyway?")
    procced <- select.list(c("Yes", "No"), graphics = FALSE)    
    if (procced!='Yes'){
      message("Submission is aborted. Have a nice day and try next time.")      
    }
  }  
  
  good <- FALSE
  while(!good) {
    
    # Get info
    name <- readline_clean("What is your name assosiated with the course?")
    takeTime <- readline_clean("How much time did you spend for this lesson? Give your time estimate expressed in minutes.")
    takeTime <- as.numeric(takeTime)
    while (!is.finite(takeTime)){
      message("Given time is not a number! Pleas try again.")
      takeTime <- readline_clean("How much time did you spend for this lesson? Give your time estimate expressed in minutes.")
      takeTime <- as.numeric(takeTime)
    }
    
    # Repeat back to them
    message("\nPlease, check your name. If it is misspecified you might lose your work. Does everything look good?\n")
    message("Your name: ", name)
    
    yn <- select.list(c("Yes", "No"), graphics = FALSE)
    if(yn == "Yes") good <- TRUE
  }
  
  sysInfo = Sys.info()
  sysUser = sysInfo["user"]
  sysName = sysInfo["sysname"]
  sysTime = Sys.time()
  
  encoded_log = submit_log(name=name, takeTime=takeTime, sysUser=sysUser, sysName=sysName, sysTime=sysTime)
  
  hrule()
  message("I just tried to open your bowser and prepare info for submition. If its OK, submit the form.")
  message("\nIf it failed, please save the string below and report the problem.\n")
  message(encoded_log)
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

submit_log <- function(...){
  
  # Please edit the link below
  pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSczLiMVNYN1csee1oUHkLUC-Lduc8Game9TdsE1G-c9-q93Xg/viewform?entry.1707857444"
  
  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }
  
  # ilit log list
  log_ = list(...)
  
  # update with actual log
  temp <- tempfile()    
  log_pure <- getLog()
  log_[names(log_pure)] = log_pure
  
  # save
  saveRDS(log_, file =temp)
  encoded_log = base64encode(temp)
  browseURL(paste0(pre_fill_link, encoded_log))
  return(encoded_log)
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}

