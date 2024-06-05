# Carregando o dataset AirPassengers
base = read.csv2("amazon.csv", sep=",")

# Transformando os meses em números
base = base %>% mutate(month = ifelse(month == "Janeiro", 1, month))
base = base %>% mutate(month = ifelse(month == "Fevereiro", 2, month))
base = base %>% mutate(month = ifelse(month == "Marco", 3, month))
base = base %>% mutate(month = ifelse(month == "Abril", 4, month))
base = base %>% mutate(month = ifelse(month == "Maio", 5, month))
base = base %>% mutate(month = ifelse(month == "Junho", 6, month))
base = base %>% mutate(month = ifelse(month == "Julho", 7, month))
base = base %>% mutate(month = ifelse(month == "Agosto", 8, month))
base = base %>% mutate(month = ifelse(month == "Setembro", 9, month))
base = base %>% mutate(month = ifelse(month == "Outubro", 10, month))
base = base %>% mutate(month = ifelse(month == "Novembro", 11, month))
base = base %>% mutate(month = ifelse(month == "Dezembro", 12, month))

base$month = as.numeric(base$month)
base$number = as.numeric(base$number)

base = base[, c("year", "month", "number")]

base = base %>% group_by(year, month) %>% summarise(number = sum(number))

# Gerando o T e I
base$T = seq(nrow(base):1)
base$I = as.factor(base$month)

# Visualização gráfica
plot(base$T,base$number,xlab="Período de Tempo", ylab="Núm. incêndios", pch = 20)
lines(base$T,base$number, col = "black")

# Modelo
modelo = lm(number ~ T + I, data=base)
summary(modelo)

# Visualização gráfica do ajuste
plot(base$T,base$number,xlab="Período de Tempo", ylab="Núm. incêndios", pch = 20)
lines(base$T,base$number, col = "black",lwd=2)
lines(base$T,modelo$fitted.values, col = "red")

# Análise de Resíduos
plot(modelo$fitted.values,modelo$residuals,xlab="valores ajustados", ylab="resíduos")
abline(0,0)
plot(modelo$fitted.values,rstandard(modelo),xlab="valores ajustados", ylab="resíduos")
abline(0,0)

# Teste de modelo
base_teste = base[-c(210:239),]
base_predicao = base[c(210:239),]

# Modelo
modelo_teste = lm(number ~ T + I, data=base_teste)
summary(modelo_teste)
previsao = predict(modelo_teste, base_predicao, interval = "prediction")
resultado = cbind(base_predicao,previsao)
resultado

# Visualização gráfica da previsão
plot(resultado$T,resultado$number,xlab="Período de Tempo", ylab="Núm. incêndios")
lines(resultado$T,resultado$number, col = "black",lwd=2)
lines(resultado$T,resultado$fit, col = "red",lwd=2)
lines(resultado$T,resultado$lwr, col = "red",lty=2,lwd=2)
lines(resultado$T,resultado$upr, col = "red",lty=2,lwd=2)

