#initialization of the ANN 
#3 inputs, 10 hidden layer nodes, 2 output
#hidden activation function is tangent function
#outupt activation function is sigmoidal function

#all the function required:

#hyperbolic tangential function
Tangent_Activation <- function(a){
  ((1+exp(-2*a))^(-1));
}

#sigmoidal function
#parameters a= the input b=steepness parameter, default=1
Sigmoidal_Activation <- function(a,b=1){
  y <-((1+exp(-b*a))^(-1));
}

#First order derivative of tangential function:
#FOD_Tangent_Activation <- function(a) y <- 2 * Tangent_Activation(a) * (1- Tangent_Activation(a))
#First order Derivative of Sigmoidal fucntion:
#FOD_Sigmoidal_Activation<- fiunction(a,b=1) y<-(2* Sigmoidal_Activation(a,b) * (1-Sigmoidal_Activation(a,b)))


#random generation of input-hidden layer weight matrix population
population_input.hidden<- list()
for (i in 1:10) {
  population_input.hidden[[i]]<-runif(30)
}

#random generation of hidden-output layer weight matrix population
population_hidden.output<- list()
for (i in 1:10) {
  population_hidden.output[[i]]<-runif(30)
}

 
WMat_input.hidden<-matrix(population_input.hidden[[1]],nrow = 3,ncol = 10, byrow = TRUE)
#print(WMat_input.hidden[[1,10]])   testing if the matrix values are accessible or  not

trainingData[8,]

#input for the hidden layer nodes calculated
hidden.input<-as.matrix(trainingData[8,]) %*% WMat_input.hidden

#activation fnction is applied to the input
hidden.output<-Tangent_Activation(hidden.input)

#multiplying the hidden layer output with the hidden-output weight matrix
WMat_hidden.output<-matrix(population_hidden.output[[1]],nrow = 10,ncol = 2, byrow = TRUE)
print(WMat_hidden.output)

#generating the input to the output layer nodes
output.sinput<-hidden.output %*% WMat_hidden.output

#applying activation function of the output layer:
final.output<-Sigmoidal_Activation(output.sinput)
final.output



#testing phase

#end