#' @export
HSF<- function(){
  if(HS==3){HS2<- readline("What is the name for health state 2?: ")} else if(HS==4){
      HS2<-(readline("What is the name for health state 2?: "))
      HS3<-(readline("What is the name for health state 3?: "))} else if
  (HS==5){  (HS2<-readline("What is the name for health state 2?: "))
    (HS3<-readline("What is the name for health state 3?: "))
    (HS4<-readline("What is the name for health state 4?: "))} else if
  (HS==6){  (HS2<-readline("What is the name for health state 2?: "))
    (HS3<-readline("What is the name for health state 3?: "))
    (HS4<-readline("What is the name for health state 4?: "))
    (HS5<-readline("What is the name for health state 5?: "))}else
    {  (HS2<-readline("What is the name for health state 2?: "))
      (HS3<-readline("What is the name for health state 3?: "))
      (HS4<-readline("What is the name for health state 4?: "))
      (HS5<-readline("What is the name for health state 5?: "))
      (HS6<-readline("What is the name for health state 6?: "))}

      if(HS==3){return(HS2)} else if(HS==4){
          return(c(HS2,HS3))} else if(HS==5){
            return(c(HS2, HS3, HS4))} else if (HS==6){
              return(c(HS2, HS3, HS4, HS4))}
}





