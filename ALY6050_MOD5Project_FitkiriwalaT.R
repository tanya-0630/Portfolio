print("Tanya Fitkiriwala")

install.packages("lpSolve")
library(lpSolve)

#large or small warehouse

prev_result <- 0
spc <- 12300
while (spc < 30000) {
  x.obj <- c(169.99,359.99,289.99,142.99)
  x.cons <- matrix(c(330,370,410,127,
                   25,40,25,1.25,
                   -0.7,-0.7,0.3,0.3,
                   0,0,-1,2,
                   -1,0,0,0,
                   0,-1,0,0,
                   0,0,-1,0,
                   0,0,0,-1), nrow = 8, byrow = TRUE)
  
  x.dir <- c("<=",
             "<=",
             "<=",
             "<=",
             "<=",
             "<=",
             "<=",
             "<=")
  
  x.rhs <- c(170000,
             spc,
             0,
             0,
             0,
             0,
             0,
             0)
  
  res <- lp("max", x.obj, x.cons, x.dir, x.rhs)$objval
  if(prev_result == res) {
    break
  }
  prev_result <- res
  spc = spc + 150
}
spc

