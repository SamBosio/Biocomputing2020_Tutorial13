#Define vaiables (not all used, just to keep track of what they represent)
#Initial normal cells
N0=99
#normal cell growth rate under treatment
rn=-.1
#mutant cell growth under treatment
rm=.05
#Carrying capacity
K1=1000000
#Number of time steps
istep1=1:450
#Initial mutant cells
M0=1
#Make empty matrix
output1=matrix(data=NA,nrow=(1+length(istep1)),ncol=3)
#Set column 1 as time
output1[,1]=c(istep1,0)
#set initial values in matrix
output1[1,2]=99
output1[1,3]=1
#for each time step, calculate individuals added and subtracted from each population
#and put the new value in the next row of the matrix
for(i in 1:length(istep1)){
  
  output1[(i+1),2]=output1[(i),2]+rn*output1[(i),2]*(1-(output1[(i),2]+output1[(i),3])/K1)
  output1[(i+1),3]=output1[(i),3]+rm*output1[(i),3]*(1-(output1[(i),3]+output1[(i),3])/K1)
}
#convert matrix to dataframe for ggplot
outputdata1=data.frame(time=output1[,1], Npopulation=output1[,2], Mpopulation=output1[,3])
library(ggplot2)
#graph the 2 populations with different-colored lines
ggplot(data=outputdata1, aes(x=time,y=population, color=variable))+
  geom_line(aes(y=Npopulation, color="Npopulation"))+
  geom_line(aes(y=Mpopulation, color="Mpopulation"))+
  theme_classic()