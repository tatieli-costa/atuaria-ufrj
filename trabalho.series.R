#========================================================
# Trabalho - Análise de Séries Temporais
# Série escolhida: nottem
#========================================================

##Carregando e conhecendo a série 

data(nottem)

y <- nottem

class(y)

start(y)

end(y)

frequency(y)

##Resumo estatístico da série

summary(y) #sumariza a série temporal

mean(y) #média

sd(y) #desvio padrão

var(y) #variância 

range(y) #intervalo de valores dos dados

length(y) #tamanho da série

##Plotando a série temporal

ts.plot(y,
        main="Temperaturas médias mensais em Nottingham",
        ylab="Temperatura",
        xlab="Ano")

##Plotando o histograma para observar a distribuição dos dados

hist(y,
     main="Histograma das temperaturas",
     ylab="Frequência",
     xlab="Temperatura")

##Plotando o Boxplot para detectar possíveis outliers

boxplot(y,
        main="Boxplot da série")

cycle(y) #série por ciclo sazonal

boxplot(y ~ cycle(y),
        xlab="Mês",
        ylab="Temperatura",
        main="Distribuição das temperaturas por mês")

##Calculando as médias mensais

tapply(y,
       cycle(y),
       mean)

##Gráfico das médias mensais

medias.mensais <- tapply(y,
                 cycle(y),
                 mean)

plot(medias.mensais,
     type="b",
     xlab="Mês",
     ylab="Temperatura média",
     main="Perfil sazonal médio") #esse é provavelmente um dos gráficos mais
#importantes da análise descritiva!!!

##Decompondo a série

dec <- decompose(y)

plot(dec) #plota um gráfico que decompõe a série temporal em: observado, 
#tendência, sazonalidade e ruído branco

##Calculando o coeficiente de variação

cv <- 100*sd(y)/mean(y) #mede a dispersão relativa

##Gerando uma tabela

media <- mean(nottem)

mediana <- median(nottem)

minimo <- min(nottem)

maximo <- max(nottem)

desvio <- sd(nottem)

estatisticas <- c(
  Media = mean(nottem),
  Mediana = median(nottem),
  DesvioPadrao = sd(nottem),
  Minimo = min(nottem),
  Maximo = max(nottem)
)

estatisticas

data.frame(
  Medida = names(estatisticas),
  Valor = as.numeric(estatisticas)
)

#_____________________________________________________________________________

z <- nottem - mean(nottem) #centralizando os dados

plot(nottem)
plot(z)

