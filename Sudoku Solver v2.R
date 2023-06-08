EasySudoku <- rbind(c(4, 0, 0, 8, 7, 0, 0, 2, 0),
                    c(0, 8, 0, 0, 0, 0, 4, 0, 0),
                    c(0, 0, 6, 3, 0, 0, 8, 0, 1),
                    c(7, 0, 0, 1, 0, 0, 0, 8, 0),
                    c(6, 1, 2, 0, 9, 8, 7, 3, 4),
                    c(0, 0, 0, 0, 6, 0, 0, 1, 9),
                    c(1, 9, 3, 4, 2, 7, 5, 0, 0),
                    c(8, 0, 7, 0, 1, 0, 3, 0, 2),
                    c(0, 2, 0, 0, 0, 3, 0, 0, 0))

MediumSudoku <- rbind(c(0, 0, 7, 0, 5, 0, 0, 0, 2),
                      c(0, 0, 0, 7, 4, 0, 0, 9, 0),
                      c(5, 8, 3, 2, 0, 0, 7, 0, 0),
                      c(0, 5, 0, 0, 0, 2, 0, 0, 0),
                      c(8, 0, 0, 4, 3, 0, 0, 0, 0),
                      c(0, 6, 0, 0, 0, 5, 3, 4, 0),
                      c(6, 0, 0, 3, 0, 0, 9, 0, 0),
                      c(0, 0, 1, 0, 0, 6, 0, 0, 7),
                      c(0, 4, 0, 0, 0, 9, 6, 8, 0))

HardSudoku <- rbind(c(8, 0, 0, 0, 9, 0, 1, 0, 5),
                    c(0, 4, 5, 0, 0, 2, 0, 0, 0),
                    c(0, 0, 6, 0, 0, 7, 0, 4, 8),
                    c(9, 8, 0, 0, 0, 0, 0, 2, 0),
                    c(0, 0, 0, 9, 0, 6, 0, 0, 3),
                    c(0, 1, 3, 0, 0, 0, 0, 0, 0),
                    c(0, 7, 0, 0, 0, 9, 4, 0, 0),
                    c(0, 0, 9, 5, 0, 0, 0, 0, 6),
                    c(0, 6, 1, 4, 0, 0, 0, 0, 0))

ExtremeSudoku <- rbind(c(0, 0, 9, 0, 2, 0, 0, 0, 1),
                       c(0, 0, 1, 0, 4, 0, 9, 0, 0),
                       c(0, 5, 6, 0, 0, 0, 0, 0, 0),
                       c(4, 0, 0, 0, 1, 0, 0, 6, 0),
                       c(9, 0, 0, 7, 0, 0, 3, 0, 0),
                       c(0, 0, 0, 4, 5, 0, 0, 0, 7),
                       c(0, 0, 0, 9, 0, 0, 6, 8, 0),
                       c(0, 0, 0, 2, 0, 0, 0, 0, 4),
                       c(7, 0, 0, 8, 0, 0, 0, 0, 0))

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

remove_numbers <- function(known_values, unknown_values) {
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
  for(i in 1:9){
    # First check along the row for one possibility
    row_values_to_replace <- which(sudoku_matrix[i,] > 9)
    row_values <- sudoku_matrix[i,sudoku_matrix[i,] <= 9]
    row_value_replacements <- sudoku_matrix[i, which(sudoku_matrix[i,] > 9)]
    
    sudoku_matrix[i, row_values_to_replace] <- remove_numbers(row_values, row_value_replacements)
    
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
  for(i in 1:9){
    col_values_to_replace <- which(sudoku_matrix[,i] > 9)
    col_values <- sudoku_matrix[sudoku_matrix[,i] <= 9, i]
    col_value_replacements <- sudoku_matrix[which(sudoku_matrix[,i] > 9), i]
    
    sudoku_matrix[col_values_to_replace, i] <- remove_numbers(col_values, col_value_replacements)
    
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
  for(i in 1:9){
    if(i <= 3){row_start = 1}else if(i <= 6){row_start = 4}else{row_start = 7}
    if(i %% 3 == 1){col_start = 1} else if(i %% 3 == 2){col_start = 4}else{col_start = 7}
    row_end <- row_start + 2
    col_end <- col_start + 2
    box <- sudoku_matrix[row_start:row_end, col_start:col_end]
    
    box_values_to_replace <- which(box > 9)
    box_values <- box[box <= 9]
    box_value_replacements <- box[which(box > 9)]
    box[box_values_to_replace] <- remove_numbers(box_values, box_value_replacements)
    
    sudoku_matrix[row_start:row_end, col_start:col_end] <- box
    
  }
  return(sudoku_matrix)
}

box_same_numbers_checker <- function(sudoku_matrix){
  for(i in 1:9){
    if(i <= 3){row_start = 1}else if(i <= 6){row_start = 4}else{row_start = 7}
    if(i %% 3 == 1){col_start = 1} else if(i %% 3 == 2){col_start = 4}else{col_start = 7}
    row_end <- row_start + 2
    col_end <- col_start + 2
    box <- sudoku_matrix[row_start:row_end, col_start:col_end]
    
    box_vec <- as.vector(box)
    duplicated_value <- box_vec[duplicated(box_vec)]
    
    if(length(duplicated_value) > 0 ){
      
      for(i in 1:length(duplicated_value)){
        
        full_value <- duplicated_value[i]
        value_locations <- which(box_vec %in% full_value)
        
        if(nchar(as.character(full_value)) == length(value_locations)){
          
          values_to_replace <- which(box > 9 & box != full_value)
          split_value <- as.numeric( strsplit(as.character(full_value), "")[[1]])
          value_replacements <- box[which(box > 9 & box!= full_value)]
          
          box[values_to_replace] <- remove_numbers(split_value, value_replacements)
          
          
          sudoku_matrix[row_start:row_end, col_start:col_end] <- box
          
        }
      }
    }
  }
  return(sudoku_matrix)
}

initial <- initial_numbers(MediumSudoku)
initial
for(i in 1:1000){
  initial <- row_checker(initial)
  initial <- column_checker(initial)
  initial <- box_checker(initial)
  initial <- box_same_numbers_checker(initial)
  i <- i +1
}
initial

