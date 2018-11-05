setwd("") #set your working directory
x<-read.csv2("data.csv")
x<-x[c(1:102),]

library(ggplot2)
library(reshape2)
library(scales)

g.1<-x[,c(6,12)]
g.2<-x[,c(6,13,14)]
g.3<-x[,c(6,15,16)]
g.4<-x[,c(6,17,18,19)]
g.5<-x[,c(6,21,22,23)]
g.6<-x[,c(6,25,26,27)]

setwd("") #where want your files to be saved?

MultiplePiePlots<-function(data){
  for (i in 1:nrow(data)){
    data.1<-as.data.frame(data[i,])
    colnames(data.1)<-c("NIDO","M","F")
    name<-as.character(data.1$NIDO)
    dd=melt(data.1, id=c("NIDO"))
    dd<-dd[,-1]
    colnames(dd)<-c("group","value")
    bp<- ggplot(dd, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")
    
    pie <- bp + coord_polar("y", start=0)
    
    pie + scale_fill_brewer("Sexo") + theme_void() +
      theme(axis.text.x=element_blank())+
      geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                    label = percent(value/sum(value))), size=5)+ggtitle(name)
    filename<-paste(name,'.pdf',sep='')
    ggsave(filename, plot = last_plot(), device = "pdf")
  }
}
MultiplePiePlots(g.2)

MultipleBarPlot_G3<-function(data){
  for (i in 1:nrow(data)){
    data.1<-as.data.frame(data[i,])
    colnames(data.1)<-c("NIDO","Mod. Familiar","Mod. Institucional")
    name<-as.character(data.1$NIDO)
    dd=melt(data.1, id=c("NIDO"))
    dd<-dd[,-1]
    colnames(dd)<-c("group","value")
    p <-ggplot(dd, aes(group, value))
    p +geom_bar(stat = "identity",fill="steelblue")+
      #geom_text(aes(label=value), vjust=-0.3, size=6)+
      theme_classic()+ggtitle(name)+ylab("")+xlab("Beneficiarios")+
      geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                    label = percent(value/sum(value))), size=5,position = "identity")+ggtitle(name)
    
    filename<-paste(name,'.pdf',sep='')
    ggsave(filename, plot = last_plot(), device = "pdf")
  }
}
MultipleBarPlot_G3(g.3)

MultipleBarPlot_G4<-function(data){
  for (i in 1:nrow(data)){
    data.1<-as.data.frame(data[i,])
    colnames(data.1)<-c("NIDO","Menores 1 año","Entre 1 y 3 años", "Entre 3 y 6 años")
    name<-as.character(data.1$NIDO)
    
    dd=melt(data.1, id=c("NIDO"))
    dd<-dd[,-1]
    colnames(dd)<-c("group","value")
    
    p <-ggplot(dd, aes(group, value))
    p +geom_bar(stat = "identity",fill="steelblue")+
      geom_text(aes(label=value), vjust=-0.3, size=6)+
      theme_classic()+ggtitle(name)+ylab("")+xlab("Beneficiarios")+
      geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                    label = percent(value/sum(value))), size=5,position = "identity")+ggtitle(name)
    
    filename<-paste(name,'.pdf',sep='')
    ggsave(filename, plot = last_plot(), device = "pdf")
  }
}
MultipleBarPlot_G4(g.4)

MultipleBarPlot_G5<-function(data){
  for (i in 1:nrow(data)){
    data.1<-as.data.frame(data[i,])
    colnames(data.1)<-c("NIDO","Afro","Indígenas", "Blanco/Mestizo")
    name<-as.character(data.1$NIDO)
    
    dd=melt(data.1, id=c("NIDO"))
    dd<-dd[,-1]
    colnames(dd)<-c("group","value")
    
    p <-ggplot(dd, aes(group, value))
    p +geom_bar(stat = "identity",fill="steelblue")+
      geom_text(aes(label=value), vjust=-0.3, size=6)+
      theme_classic()+ggtitle(name)+ylab("")+xlab("Beneficiarios")+
      geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                    label = percent(value/sum(value))), size=5,position = "identity")+ggtitle(name)
    
    filename<-paste(name,'.pdf',sep='')
    ggsave(filename, plot = last_plot(), device = "pdf")
  }
}
MultipleBarPlot_G5(g.5)

MultipleBarPlot_G6<-function(data){
  for (i in 1:nrow(data)){
    data.1<-as.data.frame(data[i,])
    colnames(data.1)<-c("NIDO","Adolescentes","18-24 años", "<24 años")
    name<-as.character(data.1$NIDO)
    
    dd=melt(data.1, id=c("NIDO"))
    dd<-dd[,-1]
    colnames(dd)<-c("group","value")
    
    p <-ggplot(dd, aes(group, value))
    p +geom_bar(stat = "identity",fill="steelblue")+
      geom_text(aes(label=value), vjust=-0.3, size=6)+
      theme_classic()+ggtitle(name)+ylab("")+xlab("Madres Beneficiarias")+
      geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                    label = percent(value/sum(value))), size=5,position = "identity")+ggtitle(name)
    
    filename<-paste(name,'.pdf',sep='')
    ggsave(filename, plot = last_plot(), device = "pdf")
  }
}
MultipleBarPlot_G6(g.6)