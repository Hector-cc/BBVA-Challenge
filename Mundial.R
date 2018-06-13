setwd("C:/Users/mb162712/Desktop/BBVA Mundial")


library(dplyr)

DATA<-data.frame(read.csv("DATA.csv", row.names=NULL, header=T))
row.names(DATA)<-DATA[,1]
DATA$Equipo<-as.character(DATA$Equipo)


marcador<- function(eq1,eq2,gp1,gp2){
  
  sum=eq1+eq2
  
  M1=rpois(1,gp1*((eq2/sum)^(1/3)))
  M2=rpois(1,gp2*((eq1/sum)^(1/3)))
  R <- c(M1, M2)
  
  if (R[1] > R[2]) {
    a<-1
  } else {
    if (R[1] == R[2]) {
      a<-2
    } else {
      a<-3
    }
  }
  R <- c(R,a)
  
  return(R)
}


Grupo <- function(Eq1,Eq2,Eq3,Eq4){
  
  P1<-filter(DATA, Equipo==Eq1)
  gp1<-as.numeric(P1[3])
  p1<-round(sum(P1[4:length(P1)])/(length(P1)-2))
  
  P2<-filter(DATA, Equipo==Eq2)
  gp2<-as.numeric(P2[3])
  p2<-round(sum(P2[4:length(P1)])/(length(P2)-2))
  
  P3<-filter(DATA, Equipo==Eq3)
  gp3<-as.numeric(P3[3])
  p3<-round(sum(P3[4:length(P1)])/(length(P3)-2))
  
  P4<-filter(DATA, Equipo==Eq4)
  gp4<-as.numeric(P4[3])
  p4<-round(sum(P4[4:length(P1)])/(length(P4)-2))
  
  
  part1<-marcador(p1,p2,gp1,gp2)
  part2<-marcador(p1,p3,gp1,gp3)
  part3<-marcador(p1,p4,gp1,gp4)
  part4<-marcador(p2,p3,gp2,gp3)
  part5<-marcador(p2,p4,gp2,gp4)
  part6<-marcador(p3,p4,gp3,gp4)
  
  par1<-matrix(c(Eq1,part1[1],part1[2], 0, 0,Eq2,part1[2],part1[1], 0, 0), nrow=2, byrow=T)
  par2<-matrix(c(Eq1,part2[1],part2[2], 0, 0,Eq3,part2[2],part2[1], 0, 0), nrow=2, byrow=T)
  par3<-matrix(c(Eq1,part3[1],part3[2], 0, 0,Eq4,part3[2],part3[1], 0, 0), nrow=2, byrow=T)
  par4<-matrix(c(Eq2,part4[1],part4[2], 0, 0,Eq3,part4[2],part4[1], 0, 0), nrow=2, byrow=T)
  par5<-matrix(c(Eq2,part5[1],part5[2], 0, 0,Eq4,part5[2],part5[1], 0, 0), nrow=2, byrow=T)
  par6<-matrix(c(Eq3,part6[1],part6[2], 0, 0,Eq4,part6[2],part6[1], 0, 0), nrow=2, byrow=T)
  
  
  T<-rbind(par1,par2,par3,par4,par5,par6)
  
  for (i in 1:12){
    T[i,5]<-1
    if(T[i,2]>T[i,3]){
      T[i,4]<-3
    }else{
      if(T[i,2]==T[i,3]){
        T[i,4]<-1
      }
    }
    
  }
  
  Equipo<-as.character(T[,1])
  GF<-as.numeric(T[,2])
  GC<-as.numeric(T[,3])
  Pts<-as.numeric(T[,4])
  Fase<-as.numeric(T[,5])
  
  TT <- data.frame(Equipo,GF,GC,Pts,Fase)
  TT$Equipo<-as.character(TT$Equipo)
  
  return(TT)
  
}







TT<-Grupo("Espana", "Iran", "Marruecos", "Portugal")
TT
str(TT)

FaseGrupos<-function(TT){
  
  H<-group_by(TT, Equipo) %>% summarise(GF = sum(GF),GC = sum(GC),Pts = sum(Pts)) %>% mutate(Diff=GF-GC) 
  H<-data.frame(H)
  H<-arrange(H, desc(Pts),desc(Diff), desc(GF))
  
  return(H)
  
}

H<-FaseGrupos(TT)
H
str(H)

liga<-function(Eq1,Eq2,fas){
  
  P1<-filter(DATA, Equipo==Eq1)
  gp1<-as.numeric(P1[3])
  Ef1<-filter(DATA, Equipo==Eq1) %>% select(Efectividad)
  p1<-round(sum(P1[4:length(P1)])/(length(P1)-2))
  
  P2<-filter(DATA, Equipo==Eq2)
  gp2<-as.numeric(P2[3])
  Ef2<-filter(DATA, Equipo==Eq2) %>% select(Efectividad)
  p2<-round(sum(P2[4:length(P1)])/(length(P2)-2))
  
  u<-runif(1)
  
  part<-marcador(p1,p2,gp1,gp2)
  
  R1<-matrix(c(part[1],part[2], 0, 0, 0), nrow=1)
  R2<-matrix(c(part[2],part[1], 0, 0, 0), nrow=1)
  
  R<-rbind(R1,R2)
  
  R[1,4]<-fas
  R[2,4]<-fas
  
  if(R[1,1]>R[1,2]){
    R[1,3]<-3
    R[1,5]<-1
  }else{
    if(R[1,1]==R[1,2]){
      R[1,3]<-1
      if (Ef1 > Ef2){
        R[1,5]<-1
      }else {
        if (Ef1 == Ef2){
          if (u >= 0.5){
            R[1,5]<-1
          }
        }
      }
    }
  }
  
  if(R[2,1]>R[2,2]){
    R[2,3]<-3
    R[2,5]<-1
  }else{
    if(R[2,1]==R[2,2]){
      R[2,3]<-1
      if (Ef1 < Ef2){
        R[2,5]<-1
      }else {
        if (Ef1 == Ef2){
          if (u < 0.5){
            R[2,5]<-1
          }
        }
      }
    }
  }
  
  
  R<-cbind(c(Eq1,Eq2),R)
  
  
  Equipo<-as.character(R[,1])
  GF<-as.numeric(R[,2])
  GC<-as.numeric(R[,3])
  Pts<-as.numeric(R[,4])
  Fase<-as.numeric(R[,5])
  Gana<-as.numeric(R[,6])
  
  R <- data.frame(Equipo,GF,GC,Pts,Fase,Gana)
  R$Equipo<-as.character(R$Equipo)
  
  R <- arrange(R, desc(Gana)) %>% select(-Gana)
  
  return(R)
  
}

Mundial<-function(){
  
  GrupA<-Grupo("Arabia Saudita", "Egipto", "Rusia", "Uruguay")
  GrupB<-Grupo("Espana", "Iran", "Marruecos", "Portugal")
  GrupC<-Grupo("Australia", "Dinamarca", "Francia", "Peru")
  GrupD<-Grupo("Argentina", "Croacia", "Islandia", "Nigeria")
  GrupE<-Grupo("Brasil", "Costa Rica", "Serbia", "Suiza")
  GrupF<-Grupo("Alemania", "Corea del Sur", "Mexico", "Suecia")
  GrupG<-Grupo("Belgica", "Inglaterra", "Panama", "Tunez")
  GrupH<-Grupo("Colombia", "Japon", "Polonia", "Senegal")
  
  GA<-FaseGrupos(GrupA)
  GB<-FaseGrupos(GrupB)
  GC<-FaseGrupos(GrupC)
  GD<-FaseGrupos(GrupD)
  GE<-FaseGrupos(GrupE)
  GF<-FaseGrupos(GrupF)
  GG<-FaseGrupos(GrupG)
  GH<-FaseGrupos(GrupH)
  
  
  Tot<-rbind(GrupA,GrupB,GrupC,GrupD,GrupE,GrupF,GrupG,GrupH)
  
  Oc1<-liga(GA$Equipo[1], GB$Equipo[2],2)
  Oc2<-liga(GC$Equipo[1], GD$Equipo[2],2)
  Oc3<-liga(GE$Equipo[1], GF$Equipo[2],2)
  Oc4<-liga(GG$Equipo[1], GH$Equipo[2],2)
  Oc5<-liga(GB$Equipo[1], GA$Equipo[2],2)
  Oc6<-liga(GD$Equipo[1], GC$Equipo[2],2)
  Oc7<-liga(GF$Equipo[1], GE$Equipo[2],2)
  Oc8<-liga(GH$Equipo[1], GG$Equipo[2],2)
  
  Tot<-rbind(Tot,Oc1,Oc2,Oc3,Oc4,Oc5,Oc6,Oc7,Oc8)
  
  Cu1<-liga(Oc1$Equipo[1],Oc2$Equipo[1],3)
  Cu2<-liga(Oc3$Equipo[1],Oc4$Equipo[1],3)
  Cu3<-liga(Oc5$Equipo[1],Oc6$Equipo[1],3)
  Cu4<-liga(Oc7$Equipo[1],Oc8$Equipo[1],3)
  
  Tot<-rbind(Tot,Cu1,Cu2,Cu3,Cu4)
  
  Sem1<-liga(Cu1$Equipo[1],Cu2$Equipo[1],4)
  Sem2<-liga(Cu3$Equipo[1],Cu4$Equipo[1],4)
  
  Tot<-rbind(Tot,Sem1,Sem2)
  
  Per<-liga(Sem1$Equipo[2],Sem2$Equipo[2],4)
  
  Final<-liga(Sem1$Equipo[1],Sem2$Equipo[1],5)
  
  Par<-rbind(Tot,Per,Final)
  
  Tot<-group_by(Par, Equipo) %>% summarise(Pts = sum(Pts),GF = sum(GF),GC = sum(GC), Fase = max(Fase)) %>% mutate(Diff=GF-GC) 
  Tot<-data.frame(Tot)
  Tot<-arrange(Tot, desc(Fase), desc(Pts),desc(Diff), desc(GF)) %>% select(-Fase)
  
  Tot<-list(Final=Tot, Partidos=Par)
  
  return(Tot)
  
}
 

Tot<-Mundial()
Tot$Final

str(Tot$Final)


BBVA_Challenge<-function(n){
  
  names<-c(1:n)
  
  T<-matrix(0,nrow=n,ncol=32)
  h<-list(0)
  
  for (i in 1:n){
    
    Mun<-Mundial()
    T[i,]<-as.character(Mun$Final$Equipo)
    h[[i]]<-Mun
    
  }
  
  
  L1<-as.character(T[,1])
  L2<-as.character(T[,2])
  L3<-as.character(T[,3])
  L4<-as.character(T[,4])
  L8<-as.character(T[,8])
  L16<-as.character(T[,16])
  L32<-as.character(T[,32])
  
  RR<-T
  
  R<-data.frame(L1,L2,L3,L4,L8,L16,L32)
  
  R$L1<-as.character(R$L1)
  R$L2<-as.character(R$L2)
  R$L3<-as.character(R$L3)
  R$L4<-as.character(R$L4)
  R$L8<-as.character(R$L8)
  R$L16<-as.character(R$L16)
  R$L32<-as.character(R$L32)
  
  R<-group_by(R, L1, L2, L3, L4, L8, L16, L32) %>% summarise(N=n()) %>% mutate(prob=N/n)
  R<-data.frame(R)
  
  Val<-arrange(R,desc(N))
  
  Prediccion<-Val[1,]
  
  H<-list(Simulaciones=h, Val=RR, Sim=Val, Prediccion=Prediccion)
  
  return(H)
}

Fin<-BBVA_Challenge(100000)

Fin$Simulaciones[[1000]]$Partidos
Fin$Simulaciones[[1000]]$Final

Fin$Val

head(Fin$Sim)

Fin$Sim

Fin$Prediccion




write.csv(Fin$Val,file="Simulaciones.csv")
