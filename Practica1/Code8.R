#################################################################
#Title: Área Cardioide por Triangulación   
#Authors: Iván Martín Gómez and Markos Aguirre Elorza
#Date: Saturday 17th February 2021
#Descriptions: 
#################################################################
funcion_Area_Cardioide_TD <- function(numeroPuntos){
  #Definimos la curva y obtenemos su área utilizando la fórmulas de teoría
  a=100;
  areaTeoria=a^2*(3*pi/2);
  theta=seq(0,2*pi,by=0.01);
  theta_discretos=seq(pi/2,3*pi/2,by=(3*pi/2-pi/2)/5);
  r=a*(1-sin(theta));
  r_discretos=a*(1-sin(theta_discretos));
  x=r*cos(theta);
  x_discretos=r_discretos*cos(theta_discretos);
  y=r*sin(theta);
  y_discretos=r_discretos*sin(theta_discretos);
  
  #Dibujamos Cardioide
  #Parte Superior Izquierda
  plot(x,y,xlim=c(min(x)-1,max(x)+1),ylim=c(min(y)-1,max(y)+1), type='l', xlab="x", ylab="y", col="blue",lwd=3);

  #Parte Izquierda
  points(x_discretos,y_discretos, col = "red", pch=19)
  
  lines(c(x_discretos[1],x_discretos[2]),c(y_discretos[1],y_discretos[2]), col = "green", pch=9);
  lines(c(x_discretos[2],x_discretos[3]),c(y_discretos[2],y_discretos[3]), col = "green", pch=9);
  lines(c(x_discretos[3],x_discretos[4]),c(y_discretos[3],y_discretos[4]), col = "green", pch=9);
  lines(c(x_discretos[4],x_discretos[5]),c(y_discretos[4],y_discretos[5]), col = "green", pch=9);
  lines(c(x_discretos[5],x_discretos[6]),c(y_discretos[5],y_discretos[6]), col = "green", pch=9);
  
  
  lines(c(x_discretos[1],x_discretos[6]),c(y_discretos[1],y_discretos[6]), col = "green", pch=9);
  lines(c(x_discretos[1],x_discretos[5]),c(y_discretos[1],y_discretos[5]), col = "green", pch=9);
  lines(c(x_discretos[1],x_discretos[4]),c(y_discretos[1],y_discretos[4]), col = "green", pch=9);
  lines(c(x_discretos[1],x_discretos[3]),c(y_discretos[1],y_discretos[3]), col = "green", pch=9);
  #Triangulo 1
  x_interseccion_1=0
  y_intersección_1=y_discretos[5];
  points(x_interseccion_1,y_intersección_1, col= "black")
  lines(c(x_interseccion_1,x_discretos[5]),c(y_intersección_1,y_discretos[5]), col = "yellow", pch=9);
  text(-50,y_discretos[5]+1,"h_1")
  text(2,-100,"b_1")
  base_Tr1=sqrt((x_discretos[1]-x_discretos[6])^2+(y_discretos[1]-y_discretos[6])^2);
  altura_Tr1=sqrt((x_interseccion_1-x_discretos[5])^2+(y_intersección_1-y_discretos[5])^2)
  area_Tr1=base_Tr1*altura_Tr1/2
  #Triangulo 2
  m_base_2=(y_discretos[5]-y_discretos[1])/(x_discretos[5]-x_discretos[1]);
  b_base_2=y_discretos[1]-m_base_2*x_discretos[1];
  m_perpendicular_2=-1/m_base_2;
  b_perpendicular_2=-m_perpendicular_2*x_discretos[4]+y_discretos[4]
  x_interseccion_2=(b_perpendicular_2-b_base_2)/(m_base_2-m_perpendicular_2)
  y_intersección_2=m_perpendicular_2*x_interseccion_2+b_perpendicular_2;
  points(x_interseccion_2,y_intersección_2, col= "black")
  lines(c(x_interseccion_2,x_discretos[4]),c(y_intersección_2,y_discretos[4]), col = "yellow", pch=9);
  text(-80,-60,"h_2")
  text(-40,-70,"b_2")
  base_Tr2=sqrt((x_discretos[1]-x_discretos[5])^2+(y_discretos[1]-y_discretos[5])^2);
  altura_Tr2=sqrt((x_interseccion_2-x_discretos[4])^2+(y_intersección_2-y_discretos[4])^2)
  area_Tr2=base_Tr2*altura_Tr2/2
  #Triangulo 3
  m_base_3=(y_discretos[4]-y_discretos[1])/(x_discretos[4]-x_discretos[1]);
  b_base_3=y_discretos[1]-m_base_3*x_discretos[1];
  m_perpendicular_3=-1/m_base_3;
  b_perpendicular_3=-m_perpendicular_3*x_discretos[3]+y_discretos[3]
  x_interseccion_3=(b_perpendicular_3-b_base_3)/(m_base_3-m_perpendicular_3)
  y_intersección_3=m_perpendicular_3*x_interseccion_3+b_perpendicular_3;
  points(x_interseccion_3,y_intersección_3, col= "black")
  lines(c(x_interseccion_3,x_discretos[3]),c(y_intersección_3,y_discretos[3]), col = "yellow", pch=9);
  text(-50,0,"h_3")
  text(-50,-25,"b_3")
  base_Tr3=sqrt((x_discretos[1]-x_discretos[4])^2+(y_discretos[1]-y_discretos[4])^2);
  altura_Tr3=sqrt((x_interseccion_3-x_discretos[3])^2+(y_intersección_3-y_discretos[3])^2)
  area_Tr3=base_Tr3*altura_Tr3/2
  #Triangulo 4
  m_base_4=(y_discretos[3]-y_discretos[1])/(x_discretos[3]-x_discretos[1]);
  b_base_4=y_discretos[1]-m_base_2*x_discretos[1];
  m_perpendicular_4=-1/m_base_4;
  b_perpendicular_4=-m_perpendicular_4*x_discretos[2]+y_discretos[2]
  x_interseccion_4=(b_perpendicular_4-b_base_4)/(m_base_4-m_perpendicular_4)
  y_intersección_4=m_perpendicular_4*x_interseccion_4+b_perpendicular_4;
  points(x_interseccion_4,y_intersección_4, col= "black")
  lines(c(x_interseccion_4,x_discretos[2]),c(y_intersección_4,y_discretos[2]), col = "yellow", pch=9);
  base_Tr4=sqrt((x_discretos[1]-x_discretos[3])^2+(y_discretos[1]-y_discretos[3])^2);
  altura_Tr4=sqrt((x_interseccion_4-x_discretos[2])^2+(y_intersección_4-y_discretos[2])^2)
  area_Tr4=base_Tr4*altura_Tr4/2
  
  area_Triangulacion=2*(area_Tr1+area_Tr2+area_Tr3+area_Tr4);
  error=100*abs(area_Triangulacion-areaTeoria)/(areaTeoria);
  cat("#######################################################\n");
  cat("#-->RESULTADOS<--\n");
  #cat("#--->Área TR1 = ", area_Tr1,"\n");
  #cat("#--->Área TR2 = ", area_Tr2,"\n");
  #cat("#--->Área TR3 = ", area_Tr3,"\n");
  #cat("#--->Área TR4 = ", area_Tr4,"\n");
  cat("#--->Área Real Cardioide = ", areaTeoria,"\n");
  cat("#--->Área Aproximada por Triangulación Cardioide = ", area_Triangulacion,"\n");
  cat("#--->Error cometido = ", error,"%\n");
  cat("#######################################################\n");
}

#Probamos la función
rm(list=ls())#Limpia Global Environment
dev.off()#Limpia los plots
funcion_Area_Cardioide_TD(5)#Llamamos a la función
