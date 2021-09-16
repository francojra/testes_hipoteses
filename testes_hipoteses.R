
# Testes de Hipóteses ----------------------------------------------------------------------------------------------------------------------

# Pacotes necessários ----------------------------------------------------------------------------------------------------------------------------------

if (!require(dplyr)) install.packages("dplyr")
library(dplyr) # Manipulação de dados                                
if (!require(car)) install.packages("car")   
library(car) # Funções para diagnóstico de regressão, homogeneidade, lm, glm                               
if (!require(dgof)) install.packages("dgof")
library(dgof) # Teste de normalidade Kolmogorov-Smirnov

# Verificando os dados ---------------------------------------------------------------------------------------------------------------------

# Verificação da normalidade por grupo usando teste de Shapiro -----------------------------------------------------------------------------

dados %>% group_by(variavel_independente) %>%
  shapiro_test(variavel_dependente)

shapiro.test(dados$variavel_dependente)

# Verificação da normalidade usando teste Kolmogorov-Smirnov ---------------------------------------------------------------------

ks.test(dados$variavel_dependente,"pnorm",mean(dados$variavel_dependente),sd(dados$variavel_dependente))

# Gráfico histograma -----------------------------------------------------------------------------------------------------------------------

hist(dados$variavel_dependente)

# Verificação da presença de outliers ------------------------------------------------------------------------------------------------------

boxplot(dados$variavel_dependente ~ dados$variavel_independente)

dados %>% group_by(variavel_independente) %>% 
  identify_outliers(variavel_dependente)

# Verificação da homogeneidade de variâncias com teste de Levene  --------------------------------------------------------------------------

leveneTest(variavel_dependente ~ variavel_independente, dados, center = mean) # Teste baseado na média

# Verificando resíduos dos dados -----------------------------------------------------------------------------------------------------------

# Verificação dos pressupostos nos resíduos ---------------------------------------------------------------------------------------

modelo <- aov(variavel_dependente ~ variavel_independente, dados) # Primeiro deve-se criar um modelo 

# Teste de normalidade para os resíduos ----------------------------------------------------------------------------------------------------

shapiro.test(modelo$residuals)

# Verificação da presença de outliers nos resíduos ------------------------------------------------------------------------------------

boxplot(modelo$residuals)

dados$Residuos <- modelo$residuals

dados %>% group_by(variavel_independente) %>% 
  identify_outliers(Residuos)

dados %>% 
  identify_outliers(Residuos)

# Verificação da homogeneidade de variâncias nos resíduos ----------------------------------------------------------------------------------

leveneTest(Residuos ~ variavel_independente, dados, center = mean)
