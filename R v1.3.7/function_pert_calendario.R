#' @title Schedule for deterministic projects
#' @description This function calculates the duration of the project, the slacks for each activity, as well as the schedule of each activity.
#' @param duration Vector with the duration for each activity.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param PRINT  Logical indicator to show the schedule represented in a graph (Default=TRUE)
#' @export
#' @references
#' \describe{
#'   \item{}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
#' }
#' @return A list of a project schedule and if PRINT=TRUE a plot of schedule.
#' @examples
#' prec1and2<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(3,2,1,1.5,4.2)
#' schedule.pert(duration,prec1and2)

schedule.pert<-function(duration,prec1and2=matrix(0),prec3and4=matrix(0),PRINT=TRUE){

  activities<-1:length(duration)
  n<-length(activities)
  tiempo.early<-numeric(n)
  tiempo.last<-numeric(n)


  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  duration<-duration[or2]
  tiempo.early<-rep(0,n)

  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
    prec<-matrix(0,nrow=nn,ncol=n-1)
    for(j in 1:nn){
      prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
    }
    prec<-prec[,as.logical(colSums(prec)),drop=FALSE]

    for(i in 1:nn) {
      tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+duration[prec[i,]]);
    }
  }

  tiempo.early<-tiempo.early[or1]



    #tiempo.early<-early.time(prec1and2,prec3and4,duration)

  tiempo.last<-rep(0,n)
  early.times<-tiempo.early[or2]
  ii<-as.logical(rowSums(precedence))
  iii<-activities[ii]
  tiempo.last[activities[ii==FALSE]]<-max(early.times+duration)
  nn<-length(iii)
  if(nn>0){
    prec<-matrix(0,nrow=nn,ncol=n-1)
    for(j in 1:nn){
      prec[j,1:length(which(precedence[iii[j],]==1))]<-which(precedence[iii[j],]==1)
    }
    prec<-prec[,as.logical(colSums(prec)),drop=FALSE]


    for(i in length(iii):1) {
      tiempo.last[iii[i]]=min(tiempo.last[prec[i,]]-duration[prec[i,]]);
    }
  }
  last.times<-tiempo.last
  tiempo.last<-tiempo.last[or1]
  duration<-duration[or1]

    #tiempo.last<-last.time(prec1and2,prec3and4,duration,tiempo.early)

    lista<-rebuild(prec1and2,prec3and4)

  if(PRINT==TRUE){

    duracion.total<-max(tiempo.early+duration)
    holguras.actividades<-round(tiempo.last-tiempo.early-duration,5)
    comienzo.temprano<-tiempo.early
    comienzo.tardio<-tiempo.early+holguras.actividades
    finalizacion.temprano<-tiempo.last-holguras.actividades
    finalizacion.tardia<-tiempo.last


    if(is.na(lista$`Type 4`[1])==FALSE){

    finalizacion.temprano[lista$`Type 4`[c(which(holguras.actividades[lista$`Type 4`[,1]]==0)),2]]<-comienzo.temprano[lista$`Type 4`[c(which(holguras.actividades[lista$`Type 4`[1,]]==0)),1]]

    comienzo.tardio[lista$`Type 4`[,1]]<-pmin(finalizacion.tardia[lista$`Type 4`[,2]],comienzo.tardio[lista$`Type 4`[,1]])
    }

    if(is.na(lista$`Type 3`[1])==FALSE){

    finalizacion.tardia[lista$`Type 3`[,2]]<-pmax(finalizacion.tardia[lista$`Type 3`[,2]],finalizacion.tardia[lista$`Type 3`[,1]])

    finalizacion.temprano[lista$`Type 3`[c(which(holguras.actividades[lista$`Type 3`[,1]]==0)),2]]<-finalizacion.tardia[lista$`Type 3`[c(which(holguras.actividades[lista$`Type 3`[,1]]==0)),1]]

    }

    if(is.na(lista$`Type 2`[1])==FALSE){

    comienzo.tardio[lista$`Type 2`[c(which(holguras.actividades[lista$`Type 2`[,2]]==0)),1]]<-comienzo.temprano[lista$`Type 2`[c(which(holguras.actividades[lista$`Type 2`[,2]]==0)),2]]

    }
    duration1<-duration[or2]
    FS<-holguras.actividades[or2]
    IS<-rep(0,n)
    if(nn>0){

      for(i in 1:nn) {
        FS[iii[i]]<-min(early.times[prec[i,]])-early.times[iii[i]]-duration1[iii[i]]
        IS[iii[i]]<-min(early.times[prec[i,]])-last.times[iii[i]]-duration1[iii[i]]
      }
    }
  FS<-FS[or1]
  IS<-pmax(IS[or1],0)

  data <- data.frame(activities,duration,comienzo.temprano,comienzo.tardio,finalizacion.temprano,finalizacion.tardia,holguras.actividades,FS,IS)
  print<-plot_ly(data) %>%
    add_segments(x = ~comienzo.temprano, xend = ~finalizacion.tardia, y = ~activities, yend = ~activities,hoverinfo = "text", color = I("black"),name = 'Trace',showlegend = TRUE) %>%
    add_markers(x = ~comienzo.temprano, y = ~activities,marker = list(size = 10),hoverinfo = "text", name = 'Earliest start date',text = ~paste("Activity: ", activities,"Earliest start date: ",comienzo.temprano), mode = 'markers',type = 'scatter') %>%
    add_markers(x = ~comienzo.tardio, y = ~activities, color = I("blue"),hoverinfo = "text",marker = list(size = 10),name = 'Latest start date',text = ~paste("Activity: ", activities,"Latest start date: ",comienzo.tardio), mode = 'markers',type = 'scatter') %>%
    add_markers(x = ~finalizacion.temprano, y = ~activities,color = I("green"),hoverinfo = "text",marker = list(size = 10), name = 'Earliest completion date',text = ~paste("Activity: ", activities,"Earliest completion date: ",finalizacion.temprano), mode = 'markers',type = 'scatter') %>%
    add_markers(x = ~finalizacion.tardia, y = ~activities,marker = list(size = 10), hoverinfo = "text",name = 'Latest completion date',text = ~paste("Activity: ", activities,"Latest completion date: ",finalizacion.tardia), mode = 'markers',type = 'scatter') %>%
    layout(title = "Schedule",yaxis=list(title = "Activities"),xaxis = list(title = "Times",zeroline=FALSE))
  colnames(data)=c("Activities"," Duration"," Earliest start time"," Latest start time"," Earliest completion time"," Latest completion time"," Slack", "Free Slack", "Independent Slack")
  lista<-list('Total duration of the project'=duracion.total,data,print)
  #print(print)

    critical.activities<-holguras.actividades%in%0
    print2<-dag.plot(prec1and2,prec3and4,critical.activities)



    return(lista)
  }
  else{
    duracion.total<-max(tiempo.early+duration)
    holguras.actividades<-tiempo.last-tiempo.early-duration
    comienzo.temprano<-tiempo.early
    comienzo.tardio<-tiempo.early+holguras.actividades
    finalizacion.temprano<-tiempo.last-holguras.actividades
    finalizacion.tardia<-tiempo.last

    if(is.na(lista$`Type 4`[1])==FALSE){

      finalizacion.temprano[lista$`Type 4`[c(which(holguras.actividades[lista$`Type 4`[,1]]==0)),2]]<-comienzo.temprano[lista$`Type 4`[c(which(holguras.actividades[lista$`Type 4`[,1]]==0)),1]]

      comienzo.tardio[lista$`Type 4`[,1]]<-pmin(finalizacion.tardia[lista$`Type 4`[,2]],comienzo.tardio[lista$`Type 4`[,1]])

    }

    if(is.na(lista$`Type 3`[1])==FALSE){

      finalizacion.tardia[lista$`Type 3`[,2]]<-pmax(finalizacion.tardia[lista$`Type 3`[,2]],finalizacion.tardia[lista$`Type 3`[,1]])

      finalizacion.temprano[lista$`Type 3`[c(which(holguras.actividades[lista$`Type 3`[,1]]==0)),2]]<-finalizacion.tardia[lista$`Type 3`[c(which(holguras.actividades[lista$`Type 3`[,1]]==0)),1]]

    }

    if(is.na(lista$`Type 2`[1])==FALSE){

      comienzo.tardio[lista$`Type 2`[c(which(holguras.actividades[lista$`Type 2`[,2]]==0)),1]]<-comienzo.temprano[lista$`Type 2`[c(which(holguras.actividades[lista$`Type 2`[,2]]==0)),2]]

    }

    duration1<-duration[or2]
    FS<-holguras.actividades[or2]
    IS<-rep(0,n)
    if(nn>0){

      for(i in 1:nn) {
        FS[iii[i]]<-min(early.times[prec[i,]])-early.times[iii[i]]-duration1[iii[i]]
        IS[iii[i]]<-min(early.times[prec[i,]])-last.times[iii[i]]-duration1[iii[i]]
      }
    }

    FS<-FS[or1]
    IS<-pmax(IS[or1],0)

    data <- data.frame(activities,duration,comienzo.temprano,comienzo.tardio,finalizacion.temprano,finalizacion.tardia,holguras.actividades,FS,IS)
    colnames(data)=c("Activities"," Duration"," Earliest start time"," Latest start time"," Earliest completion time"," Latest completion time"," Slack", "Free Slack", "Independent Slack")
    lista<-list('Total duration of the project'=duracion.total,data)
    return(lista)
  }

}

