# _Solving Sudoku_
#
# inspiration by: 
# https://github.com/dirkschumacher/r-sudoku
# https://www.youtube.com/watch?v=OyBOvtiqf1E&t=608s
#
# example sudokus:
# 
# https://abcnews.go.com/blogs/headlines/2012/06/can-you-solve-the-hardest-ever-sudoku
# 
# 8,0,0, 0,0,0, 0,0,0,
# 0,0,3, 6,0,0, 0,0,0,
# 0,7,0, 0,9,0, 2,0,0,

# 0,5,0, 0,0,7, 0,0,0,
# 0,0,0, 0,4,5, 7,0,0,
# 0,0,0, 1,0,0, 0,3,0,
# 
# 0,0,1, 0,0,0, 0,6,8,
# 0,0,8, 5,0,0, 0,1,0,
# 0,9,0, 0,0,0, 4,0,0
#
# https://www.extremesudoku.info/
#
# 4,0,0, 0,0,0, 0,0,3,
# 0,0,2, 4,0,6, 5,0,0,
# 0,6,0, 0,8,0, 0,7,0,
#
# 0,2,0, 0,9,0, 0,1,0,
# 0,0,3, 8,0,7, 6,0,0,
# 0,7,0, 0,2,0, 0,8,0,
# 
# 0,9,0, 0,6,0, 0,4,0,
# 0,0,5, 9,0,2, 8,0,0
# 2,0,0, 0,0,0, 0,0,7
#
# libraries ####
library(ompr)
library(dplyr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(plot.matrix) # for matrix plot
library(RColorBrewer) # for color map

# the base model ####

n <- 9 # 9 cells per square, row and column
numbers <- 1:n # 1 2 3 4 5 6 7 8 9

model <- MIPModel() %>%
  # The number k stored in position: i-column, j-row
  add_variable(x[i, j, k], i=1:n, j=1:n, k=1:n, type = "binary") %>%
  # no objective
  set_objective(0) %>%
  # only one number can be assigned per cell
  add_constraint(sum_expr(x[i, j, k], k=1:n) == 1, i=1:n, j=1:n) %>%
  # each number is exactly once in a row
  add_constraint(sum_expr(x[i, j, k], j=1:n) == 1, i=1:n, k=1:n) %>%
  # each number is exactly one in a column
  add_constraint(sum_expr(x[i, j, k], i=1:n) == 1, j=1:n, k=1:n) %>%
  # each 3x3 square must have all numbers
  add_constraint(sum_expr(x[i, j, k], i=1:3 + sx, j=1:3 + sy) == 1,
                 sx=seq(0, n - 3, 3), sy=seq(0, n - 3, 3), k=1:n)

model # plot model

# enter sudoku ####

sudoku <- matrix(data = c(
  8,0,0, 0,0,0, 0,0,0,
  0,0,3, 6,0,0, 0,0,0,
  0,7,0, 0,9,0, 0,2,0,
  
  0,5,0, 0,0,7, 0,0,0,
  0,0,0, 0,4,5, 7,0,0,
  0,0,0, 1,0,0, 0,3,0,
  
  0,0,1, 0,0,0, 0,6,8,
  0,0,8, 5,0,0, 0,1,0,
  0,9,0, 0,0,0, 4,0,0),
  nrow = n,
  ncol = n,
  byrow = FALSE
)
sudoku <- t(sudoku)

# plot sudoku
sudoku
plot(sudoku, digits = 0, col = c("#FFFFFF",brewer.pal(n = n, name = "Set1")))
segments(x0 = 3.5, x1 = 3.5, y0 = 0.5, y1 = 9.5, lwd = 5) # v-line
segments(x0 = 6.5, x1 = 6.5, y0 = 0.5, y1 = 9.5, lwd = 5) # v-line
segments(x0 = 0.5, x1 = 9.5, y0 = 3.5, y1 = 3.5, lwd = 5) # h-line
segments(x0 = 0.5, x1 = 9.5, y0 = 6.5, y1 = 6.5, lwd = 5) # h-line

counter <- 0 # init

modelFixed <- model
for (i in 1:n) {
  for (j in 1:n) {
    for (k in 1:n) {
      if (sudoku[i,j] == numbers[k]) {
        modelFixed <- add_constraint(modelFixed,x[i,j,k] == 1)
        counter <- counter + 1
      }
    }
  }
}
modelFixed # plot fixed model

# solve sodoku ####

result <- solve_model(modelFixed, with_ROI(solver="glpk", verbose=TRUE))
result # plot result

solution <- result %>%
  get_solution(x[i,j,k]) %>%
  filter(value > 0) %>%
  select(i,j,k)

sudokuSolved <- matrix(0, nrow = n, ncol = n) # init
for (ind in 1:(n*n)) {
  sudokuSolved[solution$i[ind],solution$j[ind]] <- solution$k[ind]
}

# plot solved sudoku
sudokuSolved
plot(sudokuSolved, digits = 0, col = brewer.pal(n = n, name = "Set1"))
segments(x0 = 3.5, x1 = 3.5, y0 = 0.5, y1 = 9.5, lwd = 5) # v-line
segments(x0 = 6.5, x1 = 6.5, y0 = 0.5, y1 = 9.5, lwd = 5) # v-line
segments(x0 = 0.5, x1 = 9.5, y0 = 3.5, y1 = 3.5, lwd = 5) # h-line
segments(x0 = 0.5, x1 = 9.5, y0 = 6.5, y1 = 6.5, lwd = 5) # h-line