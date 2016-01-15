FibReg = function(n) {
  sequence = c(0,1)
  if (n == 0|n == 1) {
    return(sequence[n+1])
  }  
  else {
    data = data.frame(matrix(round(runif(200,0,20)),100))
    data[,3] = data[,1]+data[,2]
    regression = lm(V3~X1+X2,data=data)
    return(predict(regression,setNames(data.frame(FibReg(n-1),FibReg(n-2)),c("X1","X2"))))
  }
}
