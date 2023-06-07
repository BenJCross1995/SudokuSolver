###------SUDOKU PROJECT------### 
library(dplyr)
library(base)

EasySudoku <- rbind(c(4, 0, 0, 8, 7, 0, 0, 2, 0),
                    c(0, 8, 0, 0, 0, 0, 4, 0, 0),
                    c(0, 0, 6, 3, 0, 0, 8, 0, 1),
                    c(7, 0, 0, 1, 0, 0, 0, 8, 0),
                    c(6, 1, 2, 0, 9, 8, 7, 3, 4),
                    c(0, 0, 0, 0, 6, 0, 0, 1, 9),
                    c(1, 9, 3, 4, 2, 7, 5, 0, 0),
                    c(8, 0, 7, 0, 1, 0, 3, 0, 2),
                    c(0, 2, 0, 0, 0, 3, 0, 0, 0))

SudokuMatrix <- rbind(c(0, 9, 0, 0, 0, 7, 0, 0, 0),
                      c(0, 0, 0, 0, 0, 0, 0, 4, 5),
                      c(7, 0, 0, 0, 8, 0, 0, 0, 6),
                      c(0, 0, 0, 0, 0, 5, 3, 0, 0),
                      c(0, 8, 0, 0, 0, 0, 0, 0, 9),
                      c(0, 0, 4, 9, 0, 0, 0, 6, 0),
                      c(5, 0, 1, 6, 0, 0, 0, 0, 0),
                      c(0, 0, 0, 0, 4, 0, 0, 1, 0),
                      c(0, 0, 0, 0, 0, 0, 2, 5, 7))



CompleteSudoku <- function(SudokuMatrix){
  iteration <- 1
  
  Start <- Sys.time()
  missing <- which(t(SudokuMatrix) == 0)
  print(paste0("Initially there are ", length(missing),
               " Missing Values"))
  found <- 0
  new_found <- 0
  new <- data.frame()
  
  
  while(length(missing) > 0 & iteration <= 1000){

    for(miss in missing){
      row_num <- ceiling (miss / 9)
      col_num <- ifelse(miss %% 9 == 0, 9, miss %% 9)
      box_num_row <- ceiling(row_num / 3)
      box_num_col <- ceiling(col_num / 3)
      box_contains <- setdiff(
        as.vector(
          SudokuMatrix[(3 * box_num_row - 2):(3*box_num_row),
                  (3 * box_num_col - 2):(3 * box_num_col)]
        ),
        0
      )
      possible_numbers <- setdiff(
        intersect(setdiff(c(1:9), SudokuMatrix[row_num,]),
                  setdiff(c(1:9), SudokuMatrix[,col_num])),
        box_contains
      )
      if (length(possible_numbers) == 1){
        SudokuMatrix[row_num, col_num] <- possible_numbers
        found <- found + 1
      } else {
        print(possible_numbers)
      }
    #   } else {
    #     orig <- seq(1:9)
    #     orig <- suppressWarnings(ifelse(possible_numbers == orig, possible_numbers, 0))
    #     orig <- cbind(box_num_row, box_num_col,
    #           row_num, col_num, orig)
    #     new <- rbind(new, orig)
    #   }
    }
    new_found <- found - new_found
    
    if(new_found == 0){
      break
      # new <- new %>%
      #   filter(orig != 0)
      # SudokuMatrix[row_num[new[1, 3]], col_num[new[1, 4]]] <- new[1,5]
      # print("No New Values Found")
    }
    print(paste0("After Iteration ", iteration,
                 " We Have Found ", found,
                 " Missing Values"))
    missing <- which(t(SudokuMatrix) == 0)
    iteration <- iteration + 1
  }
  
  
  print("Sudoku Complete!")
  print(Sys.time() - Start)
  
  return(SudokuMatrix)
}


CompleteSudoku(EasySudoku)

CompleteSudoku(ExpertSudoku)
warnings()
