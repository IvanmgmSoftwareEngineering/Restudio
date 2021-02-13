###############################################################################
#Title: Punto_Recta utilizando Dibujo y criterio visual Usuario
#Author: Iván Martín Gómez
#Date: Saturday 13th February 2021
#Descriptions: Dado un punto p=(p1,p2) y una recta y=mx+b, decidir si el punto
#              está por encima, debajo o sobre la recta. No se contempla el caso de
#              de rectas verticales
###############################################################################

funcion_PuntoRecta_Dibujo <- function(coordenada_x_punto = NULL,coordenada_y_punto = NULL, pendiente_recta = NULL, ordenada_origen_recta = NULL){
  
  #Control de errores
  if(is.null(ordenada_origen_recta)){
    cat("Error: no se contempla el caso de rectas verticales")
    return -1
  }
  p1=coordenada_x_punto
  p2=coordenada_y_punto
  m=pendiente_recta
  b=ordenada_origen_recta
  
}

#Probamos la función
#Ejemplo 1: p=(1,2), recta == y=3x+2
funcion_PuntoRecta(1,2,3,2)