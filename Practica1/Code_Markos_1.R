calcAreaParalelogramo<-function(u,v){
     #suma<- u-v
     c=v[2]+(u[1]/u[2])*v[1]
     pC1=c/((u[2]/u[1])+(u[1]/u[2]))
     puntCorte<-c(pC1, (u[2]/u[1])*pC1)
     altTri=sqrt((v[1]-puntCorte[1])^2+(v[2]-puntCorte[2])^2)
     baseTri=sqrt((u[1])^2+(u[2])^2)
     areaTri=(altTri*baseTri)/2
     areaParalelogramo=2*areaTri
     cat(areaParalelogramo)
   }
 calcAreaParalelogramo(c(1,2),c(-1,1))
