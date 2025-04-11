# ANÁLISE DE VOLATILIDADE DAS RECEITAS MENSAIS DO PORTO DO RECIFE 

# Instalação e carregamento dos pacotes
install.packages(c("quantmod", "rugarch", "FinTS", "tseries"))
library(quantmod)
library(rugarch)
library(FinTS)
library(tseries)
library(readxl)

# obs: Dist. normal p/ resíduos (receitas mensais de uma empresa)

# Passo 1: Carregar os dados
dados_rb <- read_excel("Porto_do_Recife.xlsx", sheet = 2)
dados_rb$Data <- as.Date(dados_rb$Data, format = "%d/%m/%Y")
dados_rb <- dados_rb[order(dados_rb$Data), ]  # Ordenar por data
serie_xts <- xts(dados_rb$RB, order.by = dados_rb$Data)
chartSeries(serie_xts, theme = "white", TA = "addVo(); addBBands()", 
            name = "Receitas Mensais", up.col = "blue4", dn.col = "red4")

# Passo 2: Calcular retornos
retornos_rb <- na.omit(ROC(serie_xts, type = "discrete"))
colnames(retornos_rb) <- "Retornos_RB"

# Passo 3: Testar estacionariedade
library(tseries)
adf.test(retornos_rb)     # H0: Série Não estacionária
                          # H1: Série Estacionária

kpss.test(retornos_rb)    # H0: Série Estacionária
                          # H0: Série Não Estacionária

# Passo 4: Análise exploratória
par(mfrow = c(2, 1))
plot(retornos_rb, main = "Variação % Retornos Mensais ", col = "blue4", lwd = 1)
hist(retornos_rb, breaks = 25, main = "Distribuição dos Retornos % Mensais", 
     xlab = "Retornos", col = "lightcoral", probability = TRUE)
lines(density(retornos_rb), col = "blue4", lwd = 2)
ArchTest(retornos_rb, lags = 12)
par(mfrow = c(1,1))

# Passo 5: Especificar modelo GARCH(1,1) com distribuição normal
especificacao_rb <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"  # Alterado para distribuição normal
)

# Passo 6: Estimar modelo GARCH
modelo_garch_rb <- ugarchfit(spec = especificacao_rb, data = retornos_rb)
print(modelo_garch_rb)

# Passo 7: Volatilidade estimada
volatilidade_estimada_rb <- sigma(modelo_garch_rb)
volatilidade_estimada_rb

# Passo 8: Plotar retornos e volatilidade
par(mfrow = c(2, 1))
plot(retornos_rb, main = "Retornos Mensais da Série RB", col = "blue4", lwd = 1)
plot(volatilidade_estimada_rb, main = "Volatilidade Estimada (GARCH(1,1))", col = "red3", lwd = 2)
par(mfrow = c(1, 1))

# Passo 9: Previsão de volatilidade para 30 períodos (meses)
previsao_volatilidade <- ugarchforecast(modelo_garch_rb, n.ahead = 30)
plot(previsao_volatilidade, which = 3, main = "Previsão de Volatilidade para 30 Meses")

# Passo 10: Calcular VaR (1 período) com distribuição normal
alpha <- 0.05
VaR_historico <- volatilidade_estimada_rb * qnorm(alpha)

# Gráfico VaR (1 período)
plot(retornos_rb, main = "Retornos com VaR (95%)", col = "blue4", lwd = 1)
lines(VaR_historico, col = "red", lwd = 2)
legend("topright", legend = c("Retornos", "VaR 95%"), col = c("blue4", "red"), lwd = c(1, 2))

# Passo 11: Calcular VaR (10 períodos)
# vol_10 <- volatilidade_estimada_rb * sqrt(10)
# VaR_10_dias <- vol_10 * qnorm(alpha)

# Gráfico VaR (10 períodos)
# plot(retornos_rb, main = "Retornos com VaR (95%) para 10 Períodos", col = "blue4", lwd = 1)
# lines(VaR_10_dias, col = "red", lwd = 2)
# legend("topright", legend = c("Retornos", "VaR 95% (10)"), col = c("blue4", "red"), lwd = c(1, 2))

# Exibir valores finais
ultima_vol <- as.numeric(tail(volatilidade_estimada_rb, 1))
VaR_1_dia <- ultima_vol * qnorm(alpha)
VaR_10_dias <- ultima_vol * sqrt(10) * qnorm(alpha)

cat("VaR para 1 período (95%):", VaR_1_dia, "\n")
# cat("VaR para 10 períodos (95%):", VaR_10_dias, "\n")

################################################################################
# Plotando Retornos e VaR + GARCH(1,1) abaixo (p/Linkedin)
# Ensaio Prévio - Série RB 2025

# Visualização da série RB original
chartSeries(serie_xts, theme = "white", TA = "addVo(); addBBands()", 
            name = "Porto do Recife", up.col = "blue4", dn.col = "red4")

# Gráfico de Retornos + VaR (95%)
par(mfrow = c(1, 1))
plot(retornos_rb, main = "Retornos Mensais com VaR (95%)", col = "blue4", lwd = 1)
graf <- lines(VaR_historico, col = "red", lwd = 2)

# Gráfico Retornos + VaR 
par(mfrow = c(2, 1))
graf

# Gráfico da volatilidade GARCH(1,1)
plot(volatilidade_estimada_rb, main = "Volatilidade Estimada - GARCH(1,1)", 
     col = "red3", lwd = 2)
par(mfrow = c(1, 1))





