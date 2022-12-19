# -------------------------------------------------------------------------
# Assignment 1
# Programmer name: 1. Hana Shah Binti Faizal Shah
#                  2. Muhammad Fakhri Azam bin Mohd Fadzil
#                  3. Mohammad Ryyan Rashid
# -------------------------------------------------------------------------

# Question 1 --------------------------------------------------------------
# store the location of the files in the variables
file1 = "/Users/ryyan/Documents/Dev/CPC351 Assign 1/PB_IN.txt"
file2 = "/Users/ryyan/Documents/Dev/CPC351 Assign 1/PB_OUT.txt"

# count the maximum number of fields in the files
no_col1 <- max(count.fields(file1, sep = ""))
no_col2 <- max(count.fields(file2, sep = ""))

# read the files
pb_in <- read.table(file1, sep = "", fill = TRUE, col.names=c(1:no_col1))
pb_out <- read.table(file2, sep = "", fill = TRUE, col.names=c(1:no_col2))

# function to check if a variable is an integer or not
isInteger <- function(var) {
  isInt <- suppressWarnings(as.integer(var) %% 1 == 0)
  # if var is an integer then isInt = TRUE, else NA
  if (is.na(isInt)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

# get the location according to PB number from ID card
get_place_name <- function(rowNo, df) {
  place <- ""
  for (j in 1:ncol(df)) {
    # if the entry is empty, end loop
    if (df[rowNo, j] == "") {
      break
    }
    # if the entry is not a number, it must be the name of the place
    if (!isInteger(df[rowNo, j])) {
      place <- paste(place, df[rowNo, j])
    }
  }
  return(place)
}

# extract the details from the ID card number
get_details <- function(var) {
  year <- as.numeric(substr(var, 1, 2))
  monthNum <- as.numeric(substr(var, 3, 4))
  date <- as.numeric(substr(var, 5, 6))
  pobCode <- as.numeric(substr(var, 7, 8))
  
  monthAbb <- month.abb[monthNum]
  dob <- paste(date, monthAbb, year)
  
  pob <- get_place(pobCode)
  
  return(list(dob = dob, pob = pob))
}

# find the PB number in the given files
get_place <- function(pobCode) {
  # look for the row containing the PB number from ID card in the PB_IN.txt file
  for (i in 1:nrow(pb_in)) {
    for (j in 1:ncol(pb_in))
      if (pb_in[i,j] == pobCode) {
        place <- get_place_name(i, pb_in)
        return(place)
      }
  }
  # look for the row containing the PB number from ID card in the PB_OUT.txt file
  for (i in 1:nrow(pb_out)) {
    for (j in 1:ncol(pb_out)) {
      if (pb_out[i, j] == pobCode) {
        place <- get_place_name(i, pb_out)
        return(place)
      }
    }
  }
  return("[ERROR]: Invalid PB Code.")
}

# get card numbers from the user
cardNums <- list()
i <- 1
while(i <= 5) {
  print (paste("Enter card number",i,":"))
  var <- readline()
  if (as.numeric(var) < 100000000000 || as.numeric(var)>999999999999) {
    print("[ERROR] - Please enter a 12 digit card number.")
  }
  else {
    cardNums <- append(cardNums, var)
    i <- i+1
  }
}

# print the details of the card numbers
for (i in 1:5) {
  details <- get_details(cardNums[[i]])
  cat("\n")
  print(paste("For ID Card Number:", cardNums[[i]], ":"))
  print(paste("Date of birth:", details[['dob']]))
  print(paste("Place of birth:", details[['pob']]))
  cat("\n")
}

# Question 2 --------------------------------------------------------------
# Brute-force approach

# install package for 'permn' function
install.packages('combinat')

# "not in" operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# x-coordinates
x <- c(60,180,80,140,20,100,200,140,40,100)
# y-coordinates
y <- c(200,200,180,180,160,160,160,140,120,120)

# Create matrix that contains the coordinates of each city
cities <- cbind(x,y)
rownames(cities) <- c("A","B","C","D","E","F","G","H","I","J")
colnames(cities) <- c("x","y")

# create an adjacency matrix for the distance between cities
compAdjMat <- function(cities) return (as.matrix(dist(cities)))

# Function to compute the tour length
distTour <- function(adjmat, tour) {
  d <- 0
  for(i in 2:nrow(adjmat)) {
    if (i == 10){
      # compute the distance from the second last city to the last city and the distance from the last city to the starting city
      d <- d + adjmat[tour[i-1],tour[i]] + adjmat[tour[i],tour[1]]
    }else{
      d <- d + adjmat[tour[i-1],tour[i]]
    }
  }
  return(d)
}

# brute force approach function
bruteForce <- function(cities) {
  
  # library
  library(combinat)
  start <- rownames(cities)[1]
  paths <- permn(rownames(cities)[-1])
  
  # compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # initial route and distance
  best_route <- NULL
  min_dist <- Inf
  
  # enumerate through all possible routes
  for(i in 1:length(paths)) {
    
    path <- paths[[i]]
    path <- c(start, path, start)
    
    # distance
    path_distance <- distTour(adjmat, path)
    
    # update distance
    if (path_distance < min_dist) {
      min_dist <- path_distance
      best_route <- path
    }
  }
  
  #return 
  return(list(distance = min_dist, path = best_route))
}


# Measure the running time of the algorithm
start_time_bf <- Sys.time()
bf <- bruteForce(cities)
end_time_bf <- Sys.time()
print(bf)

running_time_bf = difftime(end_time_bf, start_time_bf, units = "secs")
print(paste("Computational time (brute-force): ",running_time_bf, " seconds"))

# Heuristic approach

# The nearest neighbor heuristic
nearestNeighbor <- function(cities, cityNum) {
  
  # Compute adjacency matrix
  adjmat <- compAdjMat(cities)
  
  # Set the city as the starting point of the tour
  current_city <- rownames(cities)[cityNum]
  tour <- c(current_city)
  
  # Add nearest neighbor to tour
  for(i in 1:(nrow(cities)-1)){
    distances <- adjmat[current_city,-which(rownames(cities) %in% tour)]
    current_city <- names(which(distances == min(distances)))
    # Add random rule to break tie
    if (length(current_city) > 1){
      current_city <- current_city[1]
    }
    tour <- c(tour,current_city)
  }
  
  # Add last city and starting point
  last <- which(rownames(cities) %!in% tour)
  tour <- c(tour,rownames(cities)[last])
  tour <- c(tour,rownames(cities)[cityNum])
  best_tour <- tour # only one tour will be set as the best tour
  
  # Compute the tour length of the shortest round trip found
  min_distance <- distTour(adjmat,tour)
  
  # Return the round trip and its tour length
  return(list(distance = min_distance,tour = best_tour)
  )
  
}

# Measure the running time of the algorithm
start_time_hr <- Sys.time()

# Compute the round trip with the shortest tour length of each city as the starting point
for (i in 1:10){
  print(paste0("Starting city: ",rownames(cities)[i]))
  print(nearestNeighbor(cities, i))
}

end_time_hr <- Sys.time()

# Compute the running time for heuristic
running_time_hr = difftime(end_time_hr, start_time_hr, units = "secs")
print(paste("Computational time (heuristic): ",running_time_hr, " seconds"))

# Question 3 --------------------------------------------------------------

#define menu items
menuItems <- c("Cuboid", "Cube", "Cylinder", "Cone", "Sphere", "Hemi-Sphere", "Quit")

#display menu function
displayMenu <- function(options){
  
  for (i in 1:length(options)){
    cat(sprintf("%d. %s\n", i, options[i]))
  }
  
  choice <-0
  while(!any(choice == 1:length(options))&&!any(is.numeric(choice)==FALSE)){
    choice <- readline(prompt = "Please choose a shape: ")
  }
  return(choice)
}

#cuboid function
cuboid <- function(w,h,l){
  volume <- w*h*l #cuboid volume formula
  surface_area <- 2*((l*w)+(w*h)+(h*l)) #cuboid surface area formula
  my_list <- list("surface_area" = surface_area, "volume" = volume) #list of objects
  return(my_list) #return list of objects
}

#cube function
cube <- function(a){
  volume <- a^3 #cube formula
  surface_area <- 6*a^2 #cube surface area formula
  my_list <- list("surface_area" = surface_area, "volume" = volume) #list of objects
  return(my_list) #return list of objects
}

#cylinder function
cir_cylinder <- function(r,h){
  volume <- pi*(r^2)*h #cylinder volume formula
  surface_area <- (2*pi*r*h)+(2*pi*r^2) #surface area formula
  my_list <- list("surface_area" = surface_area, "volume" = volume) #list of objects
  return(my_list) #return list of objects
}

#cone function
cir_cone <- function(r,h){
  volume <- pi*(r^2)*(h/3) #cone formula
  s <- sqrt(r^2+h^2)
  surface_area <- (pi*r*s)+(pi*r^2) #surface area formula
  my_list <- list("surface_area" = surface_area, "volume" = volume) #list of objects
  return(my_list) #return list of objects
}

#sphere function
sphere <- function(r){
  volume <- (4/3)*pi*r^3 #sphere volume formula
  surface_area <- 4*pi*r^2 #sphere surface area formula
  my_list <- list("surface_area" = surface_area, "volume" = volume) #list of objects
  return(my_list) #return list of objects
}

#hemi-sphere function
hemi_sphere <- function(r){
  volume <- (2/3)*pi*r^3 #hemi-sphere volume formula
  surface_area <- 3*pi*r^2 #hemi-sphere surface area formula
  my_list <- list("surface_area" = surface_area, "volume" = volume) #list of objects
  return(my_list) #return list of objects
}



#display menu
menu <- function(){
  while(TRUE){
    
    choice <- displayMenu(menuItems)
    
    if(choice == 1){
      w <- readline(prompt = "Enter cuboid width: ")
      h <- readline(prompt = "Enter cuboid height: ")
      l <- readline(prompt = "Enter cuboid length: ")
      
      if(grepl("[^[:digit:]\\.-]",w)){
        print(paste("Invalid Width"))
        next
        menu()
      }else if(grepl("[^[:digit:]\\.-]",h)){
        print(paste("Invalid Height"))
        next
        menu()
      }else if(grepl("[^[:digit:]\\.-]",l)){
        print(paste("Invalid Length"))
        next
        menu()
      }
      
      w<-as.integer(w)
      h<-as.integer(h)
      l<-as.integer(l)
      
      mult <- cuboid(w,h,l) #call cuboid function that return a list
      print(paste("The total surface area of cuboid is: ", mult$surface_area))
      print(paste("The volume of cuboid is: ", mult$volume))
      
    }else if(choice == 2 ){
      a <- readline(prompt = "Enter cube length : ")
      
      if(grepl("[^[:digit:]\\.-]",a)){
        print(paste("Invalid Length"))
        next
        menu()
      }
      
      a<-as.integer(a)
      mult <- cube(a) #call cube function that return a list
      print(paste("The total surface area of cube is: ", mult$surface_area))
      print(paste("The volume of cube is: ", mult$volume))
      
    }else if(choice == 3 ){
      r <- readline(prompt = "Enter cylinder radius : ")
      h <- readline(prompt = "Enter cylinder height : ")
      
      if(grepl("[^[:digit:]\\.-]",r)){
        print(paste("Invalid Radius"))
        next
        menu()
      } else if(grepl("[^[:digit:]\\.-]",h)){
        print(paste("Invalid Height"))
        next
        menu()
      }
      
      r<-as.integer(r)
      h<-as.integer(h)
      mult <- cir_cylinder(r,h) #call cylinder function that return a list
      print(paste("The total surface area of cylinder is: ", mult$surface_area))
      print(paste("The volume of cylinder is: ", mult$volume))
    }else if(choice == 4){
      r <- readline(prompt = "Enter cone radius : ")
      h <- readline(prompt = "Enter cone height : ")
      
      if(grepl("[^[:digit:]\\.-]",r)){
        print(paste("Invalid Radius"))
        next
        menu()
      } else if(grepl("[^[:digit:]\\.-]",h)){
        print(paste("Invalid Height"))
        next
        menu()
      } 
      
      r<-as.integer(r)
      h<-as.integer(h)
      mult <- cir_cone(r,h) #call cone function that return a list
      
      print(paste("The total surface area of cone is: ", mult$surface_area))
      print(paste("The volume of cone is: ", mult$volume))
    }else if(choice == 5){
      r <- readline(prompt = "Enter sphere radius : ")
      
      if(grepl("[^[:digit:]\\.-]",r)){
        print(paste("Invalid Radius"))
        next
        menu()
      }
      
      r<-as.integer(r)
      mult <- sphere(r) #call sphere function that return a list
      
      print(paste("The total surface area of sphere is: ", mult$surface_area))
      print(paste("The volume of sphere is: ", mult$volume))
    }else if(choice == 6){
      r <- readline(prompt = "Enter hemi-sphere d radius : ")
      
      
      if(grepl("[^[:digit:]\\.-]",r)){
        print(paste("Invalid Radius"))
        next
        menu()
      }
      
      r<-as.integer(r)
      mult <- hemi_sphere(r) #call hemi_sphere function that return a list
      print(paste("The total surface area of hemi-sphere is: ", mult$surface_area))
      print(paste("The volume of hemi-sphere is: ", mult$volume))
    }else if(choice == 7){
      break
    }
  }
}

menu()

# Question 4 --------------------------------------------------------------
# Read text file into a dataframe
personality_test = read.table("C:/Users/Hana/Documents/USM/YEAR 3/CPC351/Assignment 1/data-final.txt",sep='\t',header=T)

# Print the number of rows and columns in the dataframe
print(paste("Number of columns:",ncol(personality_test)))
print(paste("Number of rows:",nrow(personality_test)))

# Check data type of each column of the dataframe
str(personality_test)

# Split the data into four rows, the first three rows contains 250000 rows of data
first_row <- personality_test[1:250000,]
second_row <- personality_test[250001:500000,]
third_row <- personality_test[500001:750000,]
fourth_row <- personality_test[750001:1015341,]

# Split each row into ten columns, each column contains 10 columns of data
r <- list(first_row[,c(1:10)],first_row[,c(11:20)],first_row[,c(21:30)],first_row[,c(31:40)],first_row[,c(41:50)],first_row[,c(51:60)],first_row[,c(61:70)],first_row[,c(71:80)],first_row[,c(81:90)],first_row[,c(91:100)],first_row[,c(101:110)],
          second_row[,c(1:10)],second_row[,c(11:20)],second_row[,c(21:30)],second_row[,c(31:40)],second_row[,c(41:50)],second_row[,c(51:60)],second_row[,c(61:70)],second_row[,c(71:80)],second_row[,c(81:90)],second_row[,c(91:100)],second_row[,c(101:110)],
          third_row[,c(1:10)],third_row[,c(11:20)],third_row[,c(21:30)],third_row[,c(31:40)],third_row[,c(41:50)],third_row[,c(51:60)],third_row[,c(61:70)],third_row[,c(71:80)],third_row[,c(81:90)],third_row[,c(91:100)],third_row[,c(101:110)],
          fourth_row[,c(1:10)],fourth_row[,c(11:20)],fourth_row[,c(21:30)],fourth_row[,c(31:40)],fourth_row[,c(41:50)],fourth_row[,c(51:60)],fourth_row[,c(61:70)],fourth_row[,c(71:80)],fourth_row[,c(81:90)],fourth_row[,c(91:100)],fourth_row[,c(101:110)])

# Write each partition of data into one text file
for (i in 1:44){
  if(i < 10) {
    myfile <- file.path("C:/Users/Hana/Documents/USM/YEAR 3/CPC351/Assignment 1/Personality_Test", paste0("PT_0",i, ".txt"))
  }else{
    myfile <- file.path("C:/Users/Hana/Documents/USM/YEAR 3/CPC351/Assignment 1/Personality_Test", paste0("PT_",i, ".txt"))
  }
  
  write.table(r[i], file = myfile, sep = "\t", row.names = FALSE)
}

# Set path to the folder that contains the text files
mypath = "C:/Users/Hana/Documents/USM/YEAR 3/CPC351/Assignment 1/Personality_Test"
setwd(mypath)

# Create list of text files
txt_files_ls = list.files(path=mypath, pattern="*.txt", full.names = TRUE) 

# Reading multiple textfiles and binding it by column into a dataframe
row1 <- as.data.frame(do.call(cbind, sapply(txt_files_ls[1:11], function(x) read.csv(x, sep='\t',header=TRUE,stringsAsFactors = FALSE))))
row2 <- as.data.frame(do.call(cbind, sapply(txt_files_ls[12:22], function(x) read.csv(x, sep='\t',header=TRUE,stringsAsFactors = FALSE))))
row3 <- as.data.frame(do.call(cbind, sapply(txt_files_ls[23:33], function(x) read.csv(x, sep='\t',header=TRUE,stringsAsFactors = FALSE))))
row4 <- as.data.frame(do.call(cbind, sapply(txt_files_ls[34:44], function(x) read.csv(x, sep='\t',header=TRUE,stringsAsFactors = FALSE))))

# Binding the dataframes by row and putting it in one dataframe
complete <- rbind(row1,row2,row3,row4)

# Print the number of columns and rows in the dataframe
print(paste("Number of columns:",ncol(complete)))
print(paste("Number of rows:",nrow(complete)))

# For double-checking, we will check to see if both contents of the dataframe are the same
# Set the column names of the dataframe to be the same as the original dataframe
colnames(complete) <- colnames(personality_test)

# Check data type of the columns
str(complete)

# Find the difference between the two dataframes
setdiff(complete,personality_test)

# Change data type of the columns to be the same as the original data type
complete$endelapse <- as.integer(complete$endelapse) 
complete$IPC <- as.integer(complete$IPC)

# Check if the two dataframes are identical
identical(personality_test,complete)

# Question 5 --------------------------------------------------------------

board <- as.matrix(read.table("sudoku.txt", sep=","), ncol=6, byrow = T) #initialize sudoku into a matrix from a .txt file

# Row validation function
valid_row <- function(board,row){
  temp<-list()
  for(i in 1:6){
    temp=cbind(temp, board[row, i]) #bind matrix to temporary variable
    if (sum(duplicated(temp[1,]))!=0) { #check if any number is duplicated using duplicated()
      cat(sprintf("Repeated values in row:%s\n", row))
      return(FALSE)
    }
  }
  return(TRUE) 
}

# Column validation function
valid_col <- function(board, col){
  temp<-list()
  for(i in 1:6){
    temp=rbind(temp, board[i, col]) #bind matrix to temporary variable
    if (sum(duplicated(temp[,1]))!=0) { #check if any number is duplicated using duplicated()
      cat(sprintf("Repeated values in col:%s %s", col, "\n"))
      return(FALSE)
    }
  }
  return(TRUE)
}

# Function to get the row and column of 2x3 box sector
which_sector<- function(board, row, col){
  box_row <- floor((row - 1) / 2) + 1 # Formula to get the sector row is from
  box_col <- floor((col - 1) / 3) + 1 # Formula to get the sector column is from
  
  row_from <- 2 * box_row - 1 # Formula to get the sector row is from
  row_to <- 2 * box_row # Formula to get the sector row is to
  col_from <- 3 * box_col - 2 # Formula to get the sector column is from
  col_to <-3 * box_col # Formula to get the sector column is to
  
  my_list = list("row_from"=row_from, "row_to"=row_to, "col_from"=col_from,"col_to"=col_to) #initialize list  
  return(my_list) #return all list from my_list
}

# 2x3 box sector validation function
valid_sector <- function(board, row, col){
  
  box_row <- floor((row - 1) / 2) + 1 #Formula to get the sector row 
  box_col <- floor((col - 1) / 3) + 1 #Formula to get the sector column 
  
  # Get subset matrix of box sector
  box_sector <- board[(2 * box_row - 1):(2 * box_row), (3 * box_col - 2):(3 * box_col)]
  
  temp = list()
  for(r in 1:2){
    for(c in 1:3){
      temp <- cbind(temp, box_sector[r,c]) #bind column to temp variable
    }
  }
  if(sum(duplicated(temp[1,]))!=0){ #check if any duplicate in temp variable using duplicated() function
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

valid_board <- function(board){
  
  res_row<-TRUE
  res_col<-TRUE
  res_sector_check<-TRUE
  
  # Call row validation function on each 6 row
  for (i in 1:6) {
    res1 <- valid_row(board, i) #call function of row validation
    if(res1==FALSE){
      res_row=FALSE
    }
  }
  
  # Call column validation function on each 6 column
  for (i in 1:6) {
    res2 <- valid_col(board, i) #call function of column validation
    if(res2==FALSE){
      res_col=FALSE
    }
  }
  
  # Call 2x3 box sector validation function on each sector
  for (i in 1:6) {
    for(c in 1:6){
      res3<-valid_sector(board, i,c) #call function to validate 2x3 box sector
      if(res3==FALSE){
        sector<-which_sector(board,i,c) #call which_sector to get the information of 2x3 box sector
        cat(sprintf("Repeated values in box sector row:%s:%s col:%s:%s\n", sector$row_from, sector$row_to, sector$col_from, sector$col_to))
        res_sector_check=FALSE
        break
      }
    }
  }
  
  if (res_row == TRUE  & res_col == TRUE & res_sector_check == TRUE){ #return feasible is all condition is TRUE
    print(board)
    cat("No repeated value in any rows\n")
    cat("No repeated value in any columns\n")
    cat("No repeated value in any box sector\n")
    cat("Sudoku is feasible\n")
  }else{
    # return false if all/one condition is not met
    cat("Sudoku is not feasible\n")
  }
}

valid_board(board) # Call function of board validation