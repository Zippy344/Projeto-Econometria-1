#Projeto econometria Zipman e Kevin
Sys.setenv(LANG = "en")
options(scipen = 100)

#importando bibliotecas
library(dplyr)
library(readxl)
library(corrplot)
library(lmtest)
library(sandwich)
library(lmtest)
library(stargazer)
#install.packages("xtable")
library(xtable)


#importando dados

#desemprego - Percentual da população de 16 anos e mais, economicamente ativa, desocupada
#suicidio10 - taxa multiplicada por 100000 do ano de 2010
#montando bases
setwd("C:/Users/KEVIN/Desktop/Lições FGV/3 Semestre/Projeto Econo I")
base = read.csv("baseFinal.csv")
base$homPerC = base$homicidio/base$Total

baseP = subset(base, base$Total>=30000)
baseE = base %>% filter(suicidio10!=0)

sum(base$suicidio10==0)
sum(baseP$suicidio10==0)

#modelos
mod = lm(suicidio10 ~desemprego+Homem+Urbana+idade
         +renda+homPerC+fundamentalIn+as.factor(UF), data = base)
modP = lm(suicidio10~desemprego+Homem+Urbana+idade
          +renda+homPerC+fundamentalIn+as.factor(UF), data = baseP)
modE = lm(suicidio10~desemprego+Homem+Urbana+idade
          +renda+homPerC+fundamentalIn+as.factor(UF), data = baseE)

#teste de hipotese heterocedasticidade
bp_test_mod <- bptest(mod)
bp_test_modP <- bptest(modP)
bp_test_modE <- bptest(modE)

cat("\\begin{table}[H]\n")
cat("\\centering\n")
cat("\\caption{Resultados do Teste Breusch-Pagan}\n")
cat("\\begin{tabular}{lccc}\n")
cat("\\hline\n")
cat("Modelo & Estatística & p-valor \\\\\n")
cat("\\hline\n")
cat("Modelo Base &", bp_test_mod$statistic, "&", bp_test_mod$p.value, "\\\\\n")
cat("Modelo com Variáveis Exógenas &", bp_test_modP$statistic, "&", bp_test_modP$p.value, "\\\\\n")
cat("Modelo com Variáveis Endógenas &", bp_test_modE$statistic, "&", bp_test_modE$p.value, "\\\\\n")
cat("\\hline\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")

#erros robustos
varm = vcovHC(mod, type = "HC0")
varp = vcovHC(modP, type = "HC0")
vare = vcovHC(modE, type = "HC0")

#teste de hipotese robusto
coeftest(mod, vcov. = varm)
coeftest(modP, vcov. = varp)
coeftest(modE, vcov. = vare)

#summary
summary(mod)
xtable(summary(mod))
summary(modP)
xtable(summary(modP))
summary(modE)
xtable(summary(modE))

plot(mod$fitted.values, mod$residuals)
plot(modP$fitted.values, modP$residuals)
plot(modE$fitted.values, modE$residuals)

#variaveis de interesse
varInt = data.frame(
  base$Total,
  base$Urbana,
  base$desemprego,
  base$suicidio10,
  base$idade,
  base$renda,
  base$Homem,
  base$homPerC,
  base$fundamentalIn
)
varIntP = data.frame(
  baseP$Total,
  baseP$Urbana,
  baseP$desemprego,
  baseP$suicidio10,
  baseP$idade,
  baseP$renda,
  baseP$Homem,
  baseP$homPerC,
  baseP$fundamentalIn
)

M = cor(varIntP)

png("Matriz_corr.png", width = 800, height = 800)

corrplot(M, method = "number")

dev.off()

plot(base$suicidio10, base$desemprego)

plot(base$idade, base$suicidio10)
plot(baseP$idade, baseP$suicidio10)

plot(modP$residuals)

cor(base$desemprego, mod$residuals)
cor(baseP$desemprego, modP$residuals)

cor(base$idade, base$suicidio10)
cor(baseP$idade, modP$residuals)

plot(baseP$idade, modP$residuals)

# Estabelecendo um summary
base_sel <- base[, c("suicidio10", "desemprego", "Homem", "Urbana", "idade", "renda", "homPerC", "fundamentalIn", "UF")]
baseE_sel <- baseE[, c("suicidio10", "desemprego", "Homem", "Urbana", "idade", "renda", "homPerC", "fundamentalIn", "UF")]
baseP_sel <- baseP[, c("suicidio10", "desemprego", "Homem", "Urbana", "idade", "renda", "homPerC", "fundamentalIn", "UF")]

stargazer(base_sel, baseE_sel, baseP_sel, type = "text", title = "Estatísticas Descritivas", digits = 2, out = "estatisticas_descritivas.txt")
base_desc <- stargazer(base_sel, type = "latex", title = "Base", header = FALSE)
baseE_desc <- stargazer(baseE_sel, type = "latex", title = "BaseE", header = FALSE)
baseP_desc <- stargazer(baseP_sel, type = "latex", title = "BaseP", header = FALSE)
