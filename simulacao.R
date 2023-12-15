#*******************************************************************************
#*******************************************************************************
# Modelo de simulação IS-LM em tempo discreto 
# Elaboração: Lilian Rolim
# 
# Modelo capta principais relações do modelo IS-LM para uma economia aberta no curto prazo 
# Assim, vamos desconsiderar a dinâmica da inflação e o regime de metas de inflação
#
#*******************************************************************************
#*******************************************************************************

#*******************************************************************************
# Modelo e gráficos
#*******************************************************************************

source("modelo.R") # carrega código com modelo e gráficos

#*******************************************************************************
# Parâmetros, valores iniciais
#*******************************************************************************

  c0 = 100 # consumo autônomo
  c1 = 0.6 # propensão marginal a consumir da renda disponível
  b0 = 100 # investimento autônomo
  b1 = 0.2 # propensão marginal a investir da renda
  b2 = 50 # sensibilidade do investimento à taxa de juros
  G0 = 100 # gasto do governo
  T0 = 110 # imposto agregado
  r0 = 0.05 # taxa de juros
  m0 = 2 # sensibilidade da demanda por moeda à renda
  m1 = 1000 # sensibilidade da demanda por moeda à taxa de juros
  
  Y0 = 1157.5 # renda inicial
  B0 = 200 # dívida pública inicial 
  
  delta_r = 0
  delta_G = 0
  delta_T = 0
  
  tInic = 1 # período inicial
  tMax = 200 # período final
  tshock = 100 # período do choque de política econômica

#*******************************************************************************
# Variáveis
#*******************************************************************************
  
  Y = rep(0, tMax+1) # produto
  C = rep(0, tMax+1) # consumo
  I = rep(0, tMax+1) # investimento
  Tg = rep(0, tMax+1) # tributos
  G = rep(0, tMax+1) # gasto do governo
  M = rep(0, tMax+1) # estoque de moeda
  B = rep(0, tMax+1) # dívida pública
  r = rep(0, tMax+1) # taxa de juros
  BY = rep(0, tMax+1) # relação dívida/pib
  periodo = rep(0, tMax+1) # período de simulação

#*******************************************************************************
# Simulação inicial
#*******************************************************************************

  resultados0 <- modelo( c0, c1, b0, b1, b2, G0, T0, r0, m0, m1, delta_r, delta_G, delta_T, tInic, tMax, tshock, periodo, C, I, G, Tg, Y, r, M, B, BY ) # executa rodada com parâmetros indicados acima

  cat("\n Resultados iniciais (últimos 10 períodos) \n")
  print(resultados0[ resultados0$periodo>tMax-10, ]) # valores para 10 períodos finais
  
  pdf( "ResultadosIniciais.pdf",  width = 12, height = 12 )
  par(mfrow=c(3,3))    
    graficos(resultados0) # gráfico com resltados finais
  dev.off()


#*******************************************************************************
# Parte 1: Simulação com choque de política econômica
#*******************************************************************************

  # Edite abaixo o choque de política econômica a ser implementado:
  
    delta_r = 0
    delta_G = 0
    delta_T = 0

  resultados1 <- modelo( c0, c1, b0, b1, b2, G0, T0, r0, m0, m1, delta_r, delta_G, delta_T, tInic, tMax, tshock, periodo, C, I, G, Tg, Y, r, M, B, BY ) # executa rodada com parâmetros indicados acima

  cat("\n Resultados choque de política econômica (últimos 10 períodos) \n")
  print(resultados1[ resultados1$periodo>tMax-10, ]) # valores para 10 períodos finais

  pdf( "ResultadosChoques.pdf",  width = 12, height = 12 )
  par(mfrow=c(3,3))
    graficos_choque(resultados0, resultados1) # gráfico com resultados finais
  dev.off()


#*******************************************************************************
# Parte 2: Experimento com parâmetro 
#*******************************************************************************

  # Edite abaixo apenas o parâmetro a ser alterado, substituindo pelo novo valor:

    c0 = c0 
    c1 = c1 
    b0 = b0
    b1 = b1 
    b2 = b2 
    
  # Simulamos primeiro os valores sem o choque de política econômica

    resultados2 <- modelo( c0, c1, b0, b1, b2, G0, T0, r0, m0, m1, 0, 0, 0, tInic, tMax, tshock, periodo, C, I, G, Tg, Y, r, M, B, BY ) # executa rodada com parâmetros indicados acima

    cat("\n Resultados com novo parâmetro sem choque de política econômica (últimos 10 períodos) \n")
    print(resultados2[ resultados2$periodo > tMax-10, ]) # valores para 10 períodos finais


  # E depois com os choques de política

    resultados3 <- modelo( c0, c1, b0, b1, b2, G0, T0, r0, m0, m1, delta_r, delta_G, delta_T, tInic, tMax, tshock, periodo, C, I, G, Tg, Y, r, M, B, BY ) # executa rodada com parâmetros indicados acima

    cat("\n Resultados com novo parâmetro com choque de política econômica (últimos 10 períodos) \n")
    print(resultados3[ resultados3$periodo > tMax-10, ]) # valores para 10 períodos finais

  # E geramos o gráfico comparando os valores nesta nova estrutura econômica
  
    pdf( "ResultadosChoquesParametro.pdf",  width = 12, height = 12 )
    par(mfrow=c(3,3))
      graficos_choque(resultados2, resultados3) # gráfico com resultados finais
    dev.off()
