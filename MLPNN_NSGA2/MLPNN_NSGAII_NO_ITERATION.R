#initialization of the ANN 
#3 inputs, 10 hidden layer nodes, 2 output
#hidden activation function is tangent function
#outupt activation function is sigmoidal function

#---------------------------------------------------------------
#all the function required:

#hyperbolic tangential function
Tangent_Activation <- function(a){
  (exp(a)-exp(-a))/(exp(a)+exp(-a));
}##is wrong it will be e^x-e^-x/e^x+e^-x

#sigmoidal function
#parameters a= the input b=steepness parameter, default=1
Sigmoidal_Activation <- function(a,b=1){
  y <-((1+exp(-b*a))^(-1));
}

#First order derivative of tangential function:
#FOD_Tangent_Activation <- function(a) y <- 2 * Tangent_Activation(a) * (1- Tangent_Activation(a))
#First order Derivative of Sigmoidal fucntion:
#FOD_Sigmoidal_Activation<- fiunction(a,b=1) y<-(2* Sigmoidal_Activation(a,b) * (1-Sigmoidal_Activation(a,b)))
#-------------------------------------------------------------------

#defining upper limits
Ux<-list()
#defining lower limits
Lx<-list()

#------------------------------------------------------------------------
#random generation of input-hidden layer weight matrix population
population_input.hidden<- list()
for (i in 1:20) {
  population_input.hidden[[i]]<-rnorm(30)
}

#random generation of hidden-output layer weight matrix population
population_hidden.output<- list()
for (i in 1:20) {
  population_hidden.output[[i]]<-rnorm(20)
}

#-------------------------------------------------------------------------
#### code under consturction############################################# 
#put a loop here to itereate the chromosomes 
for (i in 1:20) {
  
#############################################################################

#generating the input-hidden layer weight matrix
WMat_input.hidden<-matrix(population_input.hidden[[i]],nrow = 3,ncol = 10, byrow = TRUE)
#print(WMat_input.hidden[[1,10]])   #testing if the matrix values are accessible or  not

#generating the hidden-output weight matrix
WMat_hidden.output<-matrix(population_hidden.output[[i]],nrow = 10,ncol = 2, byrow = TRUE)
print(WMat_hidden.output)

################## to itereate through the rows in the training data set
picp<-0  
for (j in 1:200) {
#########################################################################
#print(trainingData[j,])

#input for the hidden layer nodes calculated
hidden.input<-as.matrix(normalized_data[j,]) %*% WMat_input.hidden

#activation fnction is applied to the input
hidden.output<-Tangent_Activation(hidden.input)

#multiplying the hidden layer output with the hidden-output weight matrix 
#to generate the input to the output layer nodes
output.sinput<-hidden.output %*% WMat_hidden.output

#applying activation function of the output layer:
final.output<-Sigmoidal_Activation(output.sinput)
print(final.output)

#if(data[]<final.output[1] & normalized_data[]>final.output[2])


Ux[j]<-(final.output[1])
Lx[j]<-(final.output[2])


if(data.3.a<Ux[j] && data.3.a[j]>Lx[j] ){ picp<-picp+1}

plot(final.output, xlim = c(0,2),ylim = c(0,2))
par(new=T)
}

}
#################################################################################333

  

