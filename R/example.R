library("ProjectManagement")
require(ProjectManagement)

#######Precedence Matrix

prec1and2<-matrix(0,nrow=10,ncol=10)
prec3and4<-matrix(0,nrow=10,ncol=10)
prec1and2[1,7]<-1
prec1and2[2,4]<-1
prec1and2[2,8]<-1
prec1and2[3,5]<-1
prec1and2[3,6]<-1
prec1and2[4,7]<-1
prec1and2[5,9]<-1
prec1and2[6,10]<-1
prec1and2[8,9]<-1
prec1and2[4,5]<-2
prec3and4[8,6]<-3
prec3and4[9,7]<-4

#######Durations vector

duration<-c(2,1.5,1,1.5,2,2.5,3,4,2,5)


###########DAG plot

dag.plot(prec1and2,prec3and4)

#######Schedule

schedule.pert(duration,prec1and2,prec3and4)


#######Minimum durations and costs

minimum.durations<-c(1,1,0.5,1,1,2,2,3,1,3)
activities.costs<-c(1,2,1,1,3,2,1,2,3,5)

######MCE

mce(duration,minimum.durations,prec1and2,prec3and4,activities.costs,duration.project=NULL)

######Resources

resources<-c(6,6,6,3,4,2,1,2,3,1)

######Levelelling resources

levelling.resources(duration,prec1and2,prec3and4,resources,int=0.5)

######Maximum resources

max.resources<-10

######Resources allocation

resource.allocation(duration,prec1and2,prec3and4,resources,max.resources,int=0.5)

######Actual durations and cost function

observed.duration<-c(2.5,1.5,2,2,2,6,4,6,3,5.5)
cost.function<-function(x){return(max(x-10.5,0))}

######Allocation delay costs

delay.pert(duration,prec1and2,prec3and4,observed.duration,delta=NULL,cost.function)

######Random variables

distribution<-c("TRIANGLE","EXPONENTIAL",rep("TRIANGLE",8))
values<-matrix(c(1,3,2,2/3,0,0,1/2,5/4,5/4,1/4,5/2,7/4,1,3,2,1,5,3/2,1,7,1,3,5,4,1/2,3,5/2,1,8,6),nrow=10,ncol=3,byrow=T)

######Stochastic schedule
stochastic.pert(prec1and2,prec3and4,distribution,values,percentile=0.95,plot.activities.times=c(7,8))

######Allocation stochastic delay costs

delay.stochastic.pert(prec1and2,prec3and4,distribution,values,observed.duration,percentile=NULL,delta=NULL,cost.function,compilations=1000)


