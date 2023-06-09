#--------------------------------------------------------------------------------#
#------------------------------SUDOKU PUZZLE SOLVER------------------------------#
#----------------------------------BEN J. CROSS----------------------------------#
#--------------------------------------------------------------------------------#

#----LOAD LIBRARIES----# ####
suppressPackageStartupMessages({
  library(stringr)  # Manipulate as vectors
  library(RCurl)    # To pull a sudoku from online
  library(jsonlite) # Convert online sudoku to matrix
})

#----GET SUDOKU PUZZLE----# ####

getSudoku <- function(difficulty = "easy"){
  #' This function pulls a sudoku puzzle from https://sugoku.onrender.com
  #' in difficulty easy, medium, or hard and returns is as a matrix.
  #' 
  #' @param difficulty The difficulty of the desired puzzle
  #' 
  #' @return A 9x9 sudoku matrix
  
  difficulties <- c("easy", "medium", "hard")
  if(!difficulty %in% difficulties){
    print("Enter a difficulty out of 'easy', 'medium' and 'hard'!")
    break
  } else {
    url_to_search <- paste0("https://sugoku.onrender.com/board?difficulty=",
                            difficulty)
    board <- getURL(url_to_search)
    board <- fromJSON(board)$board
    return(board)
  }
}

#----METHOD TESTING----# ####

sudoku_testing <- function(tests = 10){
  difficulties <- c("easy", "medium", "hard")
  results <- data.frame()
  for (difficulty in difficulties) {
    score <- 0
    for(i in 1:tests){
      sudoku_matrix <- getSudoku(difficulty)
      my_solution <- invisible(solve_sudoku(sudoku_matrix))
      
      for(i in 1:9){
        row[i] <- sum(my_solution[i,])
        col[i] <- sum(my_solution[,i])
      }
      
      if(sum(unique(row)) == 45 
         & sum(unique(col)) == 45){
        score <- score + 1
      }
    }
    results <- rbind(results,
                     cbind('difficulty' = difficulty,
                           'correct' = score,
                           'accuracy (%)' = score * 100 / tests))
  }
  return(results)
}

#----PLOT THE INITIAL NUMBERS----# ####

initial_numbers <- function(sudoku_matrix){
  #' This function creates an initial state sudoku matrix containing values 1-9
  #' in each position. Then takes a user defined matrix and replaces any pre-given
  #' values in the initial matrix with those given.
  #' 
  #' @param sudoku_matrix A 9x9 matrix given by a sodoku problem.
  #' 
  #' @return A 9x9 matrix with the values from the given matrix filled in
  
  # First we create a 9x9 matrix with 123456789 in each position
  initial <- matrix(rep(c(123456789), 81), nrow = 9, ncol = 9)
  
  # We then replace any values from this matrix with those that have been given in
  # the problem.
  initial[which(sudoku_matrix > 0)] <- sudoku_matrix[which(sudoku_matrix > 0)]
  
  return(initial)
}

#----SOLVER FUNCTIONS----# ####

remove_numbers <- function(known_values, unknown_values) {
  #' This function is used to remove unknwon values and replace them with
  #' known values.
  #' 
  #' @param known_values A vector of known values which will replace those
  #' that are unknown.
  #' @param unknown_values The values which we are replacing.
  #' 
  #' @return A vector of modified values
  
  
  # Remove the numbers specified in row_values from each element in initial_values
  modified_values <- lapply(unknown_values, function(x) {
    x <- as.character(x)
    for (j in known_values) {
      x <- gsub(paste0(j), "", x)
    }
    as.numeric(x)
  })
  
  # Combine the modified values into a single numeric vector
  result_unlist <- unlist(modified_values)
  
  return(result_unlist)
}

row_checker <- function(sudoku_matrix){
  #' This function checks the rows and removes any possible values from a row
  #' when we already have that value known elsewhere on the row.
  #' 
  #' @param sudoku_matrix A 9x9 matrix given by a sodoku problem.
  #' 
  #' @return A 9x9 sudoku matrix.
  
  # Create a for loop to check for single values in rows
  for(i in 1:9){
    
    # First check along the row for any values > 9 (possible values) and get their
    # position in the row
    row_values_to_replace <- which(sudoku_matrix[i,] > 9)
    
    # Then check along the row for any values < 9 (known values)
    row_values <- sudoku_matrix[i,sudoku_matrix[i,] <= 9]
    
    # we then get which values we are looking to adjust in the row
    row_value_replacements <- sudoku_matrix[i, which(sudoku_matrix[i,] > 9)]
    
    # Remove the known values from the possible option in the row.
    sudoku_matrix[i, row_values_to_replace] <- remove_numbers(row_values, row_value_replacements)
    
    #----THIS CODE NEEDS TO BE CHECKED----#
    # # Now we want to check if there are any non-single values which contain a unique digit for each row
    # row_values_to_replace_2 <- which(sudoku_matrix[i,] > 9)
    # row_values_2 <- sudoku_matrix[i, which(sudoku_matrix[i,] > 9)]
    # 
    # all_values <- unlist(strsplit(as.character(row_values_2), ""))
    # value_counts <- table(all_values)
    # 
    # unique_values <- names(value_counts[value_counts == 1])
    # 
    # if(length(unique_values) >= 1){
    # 
    #   for (value in unique_values) {
    #     row_values_2[grepl(value, row_values_2)] <- value
    #   }
    #   
    #   sudoku_matrix[i, row_values_to_replace_2] <- as.numeric(row_values_2)
    # }
      
    
  }
  return(sudoku_matrix)
}

column_checker <- function(sudoku_matrix){
  #' This function checks the rows and removes any possible values from a column
  #' when we already have that value known elsewhere on the column.
  #' 
  #' @param sudoku_matrix A 9x9 matrix given by a sodoku problem.
  #' 
  #' @return A 9x9 sudoku matrix.
  
  # Create a for loop to check for single values in columns
  for(i in 1:9){
    
    # First check along the column for any values > 9 (possible values) and get their
    # position in the column
    col_values_to_replace <- which(sudoku_matrix[,i] > 9)
    # Then check along the column for any values < 9 (known values)
    col_values <- sudoku_matrix[sudoku_matrix[,i] <= 9, i]
    # we then get which values we are looking to adjust in the column
    col_value_replacements <- sudoku_matrix[which(sudoku_matrix[,i] > 9), i]
    # Remove the known values from the possible option in the column.
    sudoku_matrix[col_values_to_replace, i] <- remove_numbers(col_values, col_value_replacements)
    
    #----THIS CODE NEEDS TO BE CHECKED----#
    # Now we want to check if there are any non-single values which contain a unique digit for each col
    # col_values_to_replace_2 <- which(sudoku_matrix[,i] > 9)
    # col_values_2 <- sudoku_matrix[which(sudoku_matrix[,i] > 9), i]
    # 
    # all_values <- unlist(strsplit(as.character(col_values_2), ""))
    # value_counts <- table(all_values)
    # 
    # unique_values <- names(value_counts[value_counts == 1])
    # 
    # if(length(unique_values) >= 1){
    #   
    #   for (value in unique_values) {
    #     col_values_2[grepl(value, col_values_2)] <- value
    #   }
    #   
    #   sudoku_matrix[col_values_to_replace_2,i] <- as.numeric(col_values_2)
    # }
    
  }
  return(sudoku_matrix)
}

box_checker <- function(sudoku_matrix){
  #' This function i used to remove any values within a box that are already known
  #' to exist within the smaller 3x3 box in a sudoku matrix.
  #' 
  #' @param sudoku_matrix A 9x9 sudoku matrix provided by the user.
  #' 
  #' @return A 9x9 sudoku matrix.
  
  # First we need to define the boxes. For my problem the top row are 1,2,3 the middle
  # row are 4,5,6 and the bottom row are boxes 7,8,9. There are probably more complex
  # ways to do this but a simple if statement works well enough
  for(i in 1:9){
    # Define the row start using if
    if(i <= 3){row_start = 1}else if(i <= 6){row_start = 4}else{row_start = 7}
    # Define the column start using modulo (can also use %in%)
    if(i %% 3 == 1){col_start = 1} else if(i %% 3 == 2){col_start = 4}else{col_start = 7}
    # The row and column end values
    row_end <- row_start + 2
    col_end <- col_start + 2
    # Create the box subset
    box <- sudoku_matrix[row_start:row_end, col_start:col_end]
    
    # Find out which values are bigger than 9 in the box and where located
    box_values_to_replace <- which(box > 9)
    # Find out which values are known already in the box
    box_values <- box[box <= 9]
    # return the values being replaced
    box_value_replacements <- box[which(box > 9)]
    
    # Remove the uknown values where values are known.
    box[box_values_to_replace] <- remove_numbers(box_values, box_value_replacements)
    
    # Replace the box in the sudoku matrix.
    sudoku_matrix[row_start:row_end, col_start:col_end] <- box
    
  }
  return(sudoku_matrix)
}

box_same_numbers_checker <- function(sudoku_matrix){
  #' This function checks each box and if a selection of numbers is returned the same
  #' amount of times as there are numbers then we make sure that another cell can't
  #' contain any of these numbers e.g. if 1,2 are proposed in 2 different boxes then 
  #' we known only those two boxes can contain either of the numbers 1 or 2 so we can
  #' successfully remove those numbers as possibilities for the other squares in the box.
  #' 
  #' @param sudoku_matrix A 9x9 matrix given by the user.
  #' 
  #' @return A 9x9 sudoku matrix.
  
  # First we need to define the boxes. For my problem the top row are 1,2,3 the middle
  # row are 4,5,6 and the bottom row are boxes 7,8,9. There are probably more complex
  # ways to do this but a simple if statement works well enough
  for(i in 1:9){
    # Define the row start using if
    if(i <= 3){row_start = 1}else if(i <= 6){row_start = 4}else{row_start = 7}
    # Define the column start using modulo (can also use %in%)
    if(i %% 3 == 1){col_start = 1} else if(i %% 3 == 2){col_start = 4}else{col_start = 7}
    # The row and column end values
    row_end <- row_start + 2
    col_end <- col_start + 2
    # Create the box subset
    box <- sudoku_matrix[row_start:row_end, col_start:col_end]
    
    # Convert the box into a vector
    box_vec <- as.vector(box)
    
    # Find any duplicated values in the box
    duplicated_value <- box_vec[duplicated(box_vec)]
    
    # If we do find any (prevent errors)
    if(length(duplicated_value) > 0 ){
      
      for(i in 1:length(duplicated_value)){
        
        # Call the value this to make easier
        full_value <- duplicated_value[i]
        # Find the positions of these duplication
        value_locations <- which(box_vec %in% full_value)
        
        # If the number of digits is the same as the number of times seen
        if(nchar(as.character(full_value)) == length(value_locations)){
          
          # Find values position over 9 which aren't the value in question
          values_to_replace <- which(box > 9 & box != full_value)
          # Split the value and make each numeric to use the remove_numbers function
          split_value <- as.numeric( strsplit(as.character(full_value), "")[[1]])
          # Get the values to replace
          value_replacements <- box[which(box > 9 & box!= full_value)]
          
          # Replace any values which have been discussed
          box[values_to_replace] <- remove_numbers(split_value, value_replacements)
          
          # Replace the box within the matrix
          sudoku_matrix[row_start:row_end, col_start:col_end] <- box
          
        }
      }
    }
  }
  return(sudoku_matrix)
}

row_remove_knowns <- function(sudoku_matrix){
  for(i in 1:9){
    # Define the row start using if
    if(i <= 3){row_start = 1}else if(i <= 6){row_start = 4}else{row_start = 7}
    # Define the column start using modulo (can also use %in%)
    if(i %% 3 == 1){col_start = 1} else if(i %% 3 == 2){col_start = 4}else{col_start = 7}
    # The row and column end values
    row_end <- row_start + 2
    col_end <- col_start + 2
    # Create the box subset
    box <- sudoku_matrix[row_start:row_end, col_start:col_end]
    
    box_vec <- as.vector(box)
    
    duplicated_value <- box_vec[duplicated(box_vec)]
    
    if(length(duplicated_value) > 0 ){
      
      for(i in 1:length(duplicated_value)){
        full_value <- duplicated_value[i]
        
        times_seen <- sum(box == full_value)
        
        for(i in 1:3){
          if(sum(box[i,] == full_value) == times_seen &
             times_seen == nchar(as.character(full_value))){
            
            row_to_check <- row_start - 1 + i
            
            values_to_replace <- which(sudoku_matrix[row_to_check,] > 9
                                       & str_detect(as.character(sudoku_matrix[row_to_check,]),
                                                    as.character(full_value))
                                       & as.character(sudoku_matrix[row_to_check,]) != as.character(full_value))
            
            sudoku_matrix[row_to_check, values_to_replace] <- as.numeric(
              str_replace_all(as.character(sudoku_matrix[row_to_check, values_to_replace]),
                              as.character(full_value), ""))
          }
        }
      }
    }
  }
  return(sudoku_matrix)
}

col_remove_knowns <- function(sudoku_matrix){
  for(i in 1:9){
    # Define the row start using if
    if(i <= 3){row_start = 1}else if(i <= 6){row_start = 4}else{row_start = 7}
    # Define the column start using modulo (can also use %in%)
    if(i %% 3 == 1){col_start = 1} else if(i %% 3 == 2){col_start = 4}else{col_start = 7}
    # The row and column end values
    row_end <- row_start + 2
    col_end <- col_start + 2
    # Create the box subset
    box <- sudoku_matrix[row_start:row_end, col_start:col_end]
    
    box_vec <- as.vector(box)
    
    duplicated_value <- box_vec[duplicated(box_vec)]
    
    if(length(duplicated_value) > 0 ){
      
      for(i in 1:length(duplicated_value)){
        full_value <- duplicated_value[i]
        
        times_seen <- sum(box == full_value)
        
        for(i in 1:3){
          if(sum(box[,i] == full_value) == times_seen &
             times_seen == nchar(as.character(full_value))){
            
            col_to_check <- col_start - 1 + i
            
            values_to_replace <- which(sudoku_matrix[,col_to_check] > 9
                                       & str_detect(as.character(sudoku_matrix[,col_to_check]),
                                                    as.character(full_value))
                                       & as.character(sudoku_matrix[,col_to_check]) != as.character(full_value))
            
            sudoku_matrix[values_to_replace,col_to_check] <- as.numeric(
              str_replace_all(as.character(sudoku_matrix[values_to_replace, col_to_check]),
                              as.character(full_value), ""))
          }
        }
      }
    }
  }
  return(sudoku_matrix)
}

single_value_rows <- function(sudoku_matrix){
  for(i in 1:9){
    values_to_compare <- sudoku_matrix[i, which(sudoku_matrix[i,] > 9)]
    values <- unlist(strsplit(as.character(values_to_compare), ""))
    replacing_values <- names(which(table(values) ==1))
    if(length(replacing_values) >= 1){
      for(num in replacing_values){
        replace_where <- str_detect(as.character(sudoku_matrix[i,]),
                                    num)
        sudoku_matrix[i, replace_where == TRUE] <- as.numeric(num)
      }
    }
  }
  return(sudoku_matrix)
}

single_value_cols <- function(sudoku_matrix){
  for(i in 1:9){
    values_to_compare <- sudoku_matrix[which(sudoku_matrix[,i] > 9), i]
    values <- unlist(strsplit(as.character(values_to_compare), ""))
    replacing_values <- names(which(table(values) ==1))
    if(length(replacing_values) >= 1){
      for(num in replacing_values){
        replace_where <- str_detect(as.character(sudoku_matrix[,i]),
                                    num)
        sudoku_matrix[replace_where == TRUE, i] <- as.numeric(num)
      }
    }
  }
  return(sudoku_matrix)
}

#----REMAINING FUNCTION LOGIC----# ####

# If there are duplicate possibilities in a box and they're all on the same
# row or column then those values cannot be seen anywhere else in that row
# or column.

# If there is a unique possibility within a vector of possibilities on a row
# or column or box then that must be the value in that position e.g. if 123 is in a
# position and there are no other possible 3's in the row or column or box then the
# value must be a 3.

#----SOLVE SUDOKU----# ####
solve_sudoku <- function(sudoku_matrix){
  initial <- initial_numbers(sudoku_matrix)
  
  initial_missing <- sum(as.vector(initial) > 9)
  
  found <- 0
  new_found <- 0
  for(i in 1:50){
    missing <- sum(as.vector(initial) > 9)
    initial <- row_checker(initial)
    initial <- column_checker(initial)
    initial <- box_checker(initial)
    initial <- box_same_numbers_checker(initial)
    initial <- row_remove_knowns(initial)
    initial <- column_checker(initial)
    initial <- col_remove_knowns(initial)
    initial <- row_checker(initial)
    
    found <- missing - sum(as.vector(initial) > 9, na.rm = TRUE)
    
    if(found > 0)
    { i <- i +1
    } else {
      initial <- single_value_rows(initial)
      initial <- column_checker(initial)
      initial <- single_value_cols(initial)
      initial <- row_checker(initial)
      i <- i+1
    }
      
    }
  return(initial)
}

#----TEST USING SINGLE SUDOKU----# ####
library(sudoku)
todays_sudoku <- fetchSudokuUK()

solve_sudoku(todays_sudoku)
solveSudoku(todays_sudoku)

#----TEST USING MY METHOD----# ####
sudoku_testing(10)

