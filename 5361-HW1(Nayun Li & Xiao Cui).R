#define the function of Monte Carlo methods 
distribution_function<-function(i,num){
  x     <- rnorm(num)
  sum.x <- 0
  for (j in 1:length(x)){
    if (x[j] <= i){
      sum.x <- sum.x+1
    }
  }
  return(sum.x/num)
}

#print value table of the results
i.test      <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)
num.test    <- c(100, 1000, 10000)
results_table <- matrix(NA, nrow = 9, ncol = 3, byrow = T, dimnames <- list(c(" t=0.0", "t=0.67", "t=0.84", "t=1.28", "t=1.65", "t=2.32", "t=2.58", "t=3.09", "t=3.72"),
                                                                            c('N=10^2', 'N=10^3', 'N=10^4')))
for(num in num.test){
    for(i in i.test){
      r_index = which(i.test == i)
      c_index = which(num.test == num)
      results_table[r_index, c_index] <- distribution_function(i, num)
    }
}
True_value <- c(pnorm(0.0), pnorm(0.67), pnorm(0.84), pnorm(1.28), pnorm(1.65), pnorm(2.32), pnorm(2.58), pnorm(3.09), pnorm(3.72))
results_table <- cbind(results_table, True_value)

#box plot the bias
result1 <- array()
result2 <- array()
result3 <- array()
result4 <- array()
result5 <- array()
result6 <- array()
result7 <- array()
result8 <- array()
result9 <- array()

for (k in 1:100){
  result1[k] <- distribution_function(0.00,100) - pnorm(0.00)
  result2[k] <- distribution_function(0.67,100) - pnorm(0.67)
  result3[k] <- distribution_function(0.84,100) - pnorm(0.84)
  result4[k] <- distribution_function(1.28,100) - pnorm(1.28)
  result5[k] <- distribution_function(1.65,100) - pnorm(1.65)
  result6[k] <- distribution_function(2.32,100) - pnorm(2.32)
  result7[k] <- distribution_function(2.58,100) - pnorm(2.58)
  result8[k] <- distribution_function(3.09,100) - pnorm(3.09)
  result9[k] <- distribution_function(3.72,100) - pnorm(3.72)
}

test.matrix1 <- matrix(cbind(result1,result2,result3,result4,result5,result6,result7,result8,result9),
                    ncol = 9, nrow = 100)
colnames(test.matrix1) <- c(0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)

for (k in 1:100){
  result1[k] <- distribution_function(0.00, 1000) - pnorm(0.00)
  result2[k] <- distribution_function(0.67, 1000) - pnorm(0.67)
  result3[k] <- distribution_function(0.84, 1000) - pnorm(0.84)
  result4[k] <- distribution_function(1.28, 1000) - pnorm(1.28)
  result5[k] <- distribution_function(1.65, 1000) - pnorm(1.65)
  result6[k] <- distribution_function(2.32, 1000) - pnorm(2.32)
  result7[k] <- distribution_function(2.58, 1000) - pnorm(2.58)
  result8[k] <- distribution_function(3.09, 1000) - pnorm(3.09)
  result9[k] <- distribution_function(3.72, 1000) - pnorm(3.72)
}

test.matrix2 <- matrix(cbind(result1, result2, result3, result4, result5, result6, result7, result8, result9),
                     ncol = 9, nrow = 100)
colnames(test.matrix2) <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)

for (k in 1:100){
  result1[k] <- distribution_function(0.00,10000) - pnorm(0.00)
  result2[k] <- distribution_function(0.67,10000) - pnorm(0.67)
  result3[k] <- distribution_function(0.84,10000) - pnorm(0.84)
  result4[k] <- distribution_function(1.28,10000) - pnorm(1.28)
  result5[k] <- distribution_function(1.65,10000) - pnorm(1.65)
  result6[k] <- distribution_function(2.32,10000) - pnorm(2.32)
  result7[k] <- distribution_function(2.58,10000) - pnorm(2.58)
  result8[k] <- distribution_function(3.09,10000) - pnorm(3.09)
  result9[k] <- distribution_function(3.72,10000) - pnorm(3.72)
}

test.matrix3 <- matrix(cbind(result1, result2, result3, result4, result5, result6, result7, result8, result9),
                     ncol = 9, nrow = 100)
colnames(test.matrix3) <- c(0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72)

#output the table and the figures
par(mfrow = c(3, 1))
boxplot(test.matrix1, main = 'N=100', ylim = c(-0.12, 0.12), xlab = 't', ylab = 'Bias', col = 'red')
boxplot(test.matrix2, main = 'N=1000', ylim = c(-0.12,0.12), xlab = 't', ylab = 'Bias', col = 'blue')
boxplot(test.matrix3, main = 'N=10000', ylim = c(-0.12,0.12), xlab = 't', ylab = 'Bias', col = 'yellow')
print(results_table)