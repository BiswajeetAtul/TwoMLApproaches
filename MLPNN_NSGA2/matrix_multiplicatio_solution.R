matrixA<-matrix(runif(6),nrow = 3,ncol = 2)
matrixB<-matrix(runif(6),nrow=2,ncol = 3)


matrixA

matrixB

matrixA %*% matrixB

matrixB*matrixA

matrixA*t(matrixB)


#here is what happend when i tried what i wanted it do
> trainingData[5,]*WMat_input.hidden
windspd  windspd1 windspd2
5 0.7560336 0.9469494 7.267939
> matrixA<-matrix(runif(6),nrow = 3,ncol = 2)
> matrixB<-matrix(runif(6),nrow=2,ncol = 3)
> matrixA
[,1]        [,2]
[1,] 0.8211942 0.931756298
[2,] 0.4897188 0.220018834
[3,] 0.1579891 0.003135268
> matrixB
[,1]      [,2]      [,3]
[1,] 0.3772371 0.9725348 0.5156528
[2,] 0.5462535 0.7312752 0.3399391
> matrixA*matrixB
Error in matrixA * matrixB : non-conformable arrays
> matrixB*matrixA
Error in matrixB * matrixA : non-conformable arrays
> matrixA*t(matrixB)
[,1]      [,2]
[1,] 0.30978496 0.5089752
[2,] 0.47626853 0.1608943
[3,] 0.08146753 0.0010658
> ginv
Error: object 'ginv' not found
> /ginv
Error: unexpected '/' in "/"
> ?ginv
No documentation for 'ginv' in specified packages and libraries:
  you could try '??ginv'
> ??ginv
> matrixA %*% matrixB
[,1]      [,2]       [,3]
[1,] 0.81876014 1.4800103 0.74019147
[2,] 0.30492617 0.6371629 0.32731783
[3,] 0.06131202 0.1559427 0.08253333
> trainingData[5,] %*% WMat_input.hidden
Error in trainingData[5, ] %*% WMat_input.hidden : 
  requires numeric/complex matrix/vector arguments
> trainingData[5,1]
[1] 18
> trainingData[5,]
windspd windspd1 windspd2
5      18       15       17
> matrixA<-matrix(trainingData[5,],nrow = 1,ncol = 3, byrow = TRUE)
> matrixB<-WMat_input.hidden
> matrixA
[,1] [,2] [,3]
[1,] 18   15   17  
> matrixB
[,1]      [,2]       [,3]      [,4]      [,5]      [,6]       [,7]      [,8]       [,9]
[1,] 0.04200187 0.4814222 0.07902679 0.2276124 0.4889741 0.6940353 0.11774965 0.7351893 0.49246606
[2,] 0.06312996 0.2971923 0.06043623 0.4991782 0.5595915 0.6388784 0.02898601 0.6376774 0.62477115
[3,] 0.42752583 0.6767308 0.77924181 0.9871657 0.8665887 0.2312771 0.55159511 0.7319346 0.04694659
[,10]
[1,] 0.7049736
[2,] 0.3005876
[3,] 0.3988355
> matrixA %*% matrixB
Error in matrixA %*% matrixB : 
  requires numeric/complex matrix/vector arguments
> matrixB*matrixA
Error in matrixB * matrixA : non-numeric argument to binary operator
> matrixA*t(matrixB)
Error in matrixA * t(matrixB) : non-numeric argument to binary operator
> matrixA*(matrixB)
Error in matrixA * (matrixB) : non-numeric argument to binary operator
> as.matrix(trainingData[5,])%*%matrixB
[,1]     [,2]     [,3]     [,4]     [,5]     [,6]    [,7]     [,8]     [,9]    [,10]
5 8.970922 24.62791 15.57614 28.36651 31.92741 26.00752 11.9314 35.24146 19.03405 23.97854





###-----------------------dummy code for iterating through the population and for each population all the rows of the dataset.
for (i in population) { ####iterating can be done using iter()
  i_hmatrix formed from population_i
  h_omatrix formed from population_i
  for(j in dataset){
    ##multiply 
    ##
    ##
    ##
    ##
    ##
    ##
    ##
    ##
    ##
    ##
    ##
    ##
  }
}