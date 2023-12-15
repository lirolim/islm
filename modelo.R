#*******************************************************************************
# Modelo de simulação
#*******************************************************************************

modelo <- function( c0, c1, b0, b1, b2, G0, T0, r0, m0, m1, delta_r, delta_G, delta_T, tInic, tMax, tshock, periodo, C, I, G, Tg, Y, r, M, B, BY ){ 
  
  # atualizando valores iniciais (t = 1):
  
    Y[ 1 ] = Y0
    Tg[ 1 ] = T0
    B[ 1 ] = B0
    r[ 1 ] = r0
    G[ 1 ] = G0
    
  # rodar para cada período

   for( t in (tInic+1) : (tInic+tMax) ){ 
     
      # Consumo
      C[ t ] = c0 + c1 * (Y[ t-1 ] - Tg[ t-1 ])

      # Investimento
      I[ t ] = b0 + b1 * Y[ t-1 ] - b2 * r[ t-1 ]

      # Gasto do governo 
      if(t == tshock){
        G[ t ] = G[ t-1 ] + delta_G
      }else{
        G[ t ] = G[ t-1 ]
      }

      # Produto
      Y[ t ] = C[ t ] + I[ t ] + G[ t ]

      # Tributação
      if(t == tshock){
        Tg[ t ] = Tg[ t-1 ] + delta_T
      }else{
        Tg[ t ] = Tg[ t-1 ]
      }

      
      # Taxa de juros
      if(t == tshock){
        r[ t ] = r[ t-1 ] + delta_r
      }else{
        r[ t ] = r[ t-1 ]
      }

      # Demanda por moeda em termos reais
      M[ t ] = m0 * Y[ t ] - m1 * r[ t ]

      # Dívida do governo
      B[ t ] = ( 1 + r[t] ) * B[ t-1 ] + G[ t ] - Tg[ t ]

      # Coeficiente de endividamento
      BY[ t ] = B[ t ] / Y[ t ]
      
      # Periodo 
      
      periodo[ t ] = t-1

    } 
  

  resultado <- subset( data.frame(periodo, Y, C, I, G, Tg, r, M, B, BY), periodo > 0 )

  return(resultado)
}

#*******************************************************************************
# Gráfico
#*******************************************************************************

graficos <- function( resultado ){
  
  graficos <- c("Y",  "C", "I", "G", "Tg", "r", "M", "B", "BY")
  
  for(k in 1: length(graficos)){
    plot(resultado[ ,"periodo"], resultado[ , graficos[k] ], main =  graficos[k], type = "l", xlab = "Periodos", ylab="")
  }

}

graficos_choque <- function( resultado, resultadochoque ){
  
  graficos <- c("Y", "C", "I", "G", "Tg", "r", "M", "B", "BY")
    
  for(k in 1: length(graficos)){
    plot(resultado[ ,"periodo"], resultado[ , graficos[k] ], ylim= c( min(resultado[ , graficos[k] ], resultadochoque[ , graficos[k] ])*0.95, max(resultado[ , graficos[k] ], resultadochoque[ , graficos[k] ])*1.05), main =  graficos[k], type = "l", xlab = "Periodos", ylab="", col = "blue")
    lines(resultadochoque[ ,"periodo"], resultadochoque[ , graficos[k] ], col = "red", type = "l")
    legend("bottomleft", c("Sem choque","Com choque"), lwd=2, col=c("blue","red"), box.lty=0)
  }
    
}
