# https://yampa.app/blog/empreendedorismo-no-pcd/
# https://agenciadenoticias.ibge.gov.br/agencia-noticias/2012-agencia-de-noticias/noticias/37317-pessoas-com-deficiencia-tem-menor-acesso-a-educacao-ao-trabalho-e-a-renda
# https://agenciagov.ebc.com.br/noticias/202403/levantamento-do-esocial-aponta-545-9-mil-trabalhadores-com-deficiencia-no-mercado-de-trabalho-no-brasil#:~:text=A%20média%20salarial%20de%20uma,de%20R%24%201.637%2C50.

# analise do default
pop_sol_cred<-10000
set.seed(999)
n<-6048
bom.n<-rbinom(n,1,.85)
bom.n
table(bom.n)
str(bom.n)
mean(bom.n)

class_pg <- factor(bom.n,
                   levels = c(0,1),
                   labels = c("mau","bom"),
                   ordered = TRUE 
)

str(class_pg)

class_pg

summary(class_pg)

table(class_pg)

data.default<-data.frame(class_pg)

data.default

tail(data.default)

head(data.default)

data.default<-data.frame(table(class_pg))
data.default

colnames(data.default)[2]<-"Freq_absoluta"
data.default

data.default<-data.frame(id=1:2,data.default)
data.default

Freq_relativa<-
  round(data.default$Freq_absoluta/sum(data.default$Freq_absoluta)*100,2)

data.default

data.default$id.1<-NULL
data.default

Freq_relativa

Freq_relativa<-data.frame(id=1:2,Freq_relativa)

Freq_relativa

table_padrão<-data.frame(merge(data.default,Freq_relativa))
table_padrão

barplot(table_padrão$Freq_relativa,xlab= "Tipo de Tomador
Crédito",ylab="Frequência_Relativa",col=c("seagreen", "yellowgreen"))

class_pg

prop.table(table(class_pg))

barplot(prop.table(table(class_pg)),xlab="Tipo de tomador de
crédito",ylab="Frequência Absoluta",col=c("red","blue"))

# Analise dos pcd
# correspondem a 9% da nossa população br
# 29% pcd trabalham
# 85,52% é mau pagador
# 63% é homem, 37% é mulher
# 47% tem mais de 60 anos 
pop_sol_cred<-10000
set.seed(999)
n <- pop_sol_cred * 0.09

idade <- c(
  sample(2:9, n * 0.04, replace = TRUE), # 2 a 9 - 4%
  sample(10:19,n * 0.05, replace = TRUE), # 10 a 19 - 5%
  sample(20:29,n * 0.06, replace = TRUE), # 20 a 29 - 6%
  sample(30:39,n * 0.08, replace = TRUE), # 30 a 39 - 8%
  sample(40:49,n * 0.12, replace = TRUE), # 40 a 49 - 12%
  sample(50:59,n * 0.17, replace = TRUE), # 50 a 59 - 17%
  sample(60:69,n * 0.18, replace = TRUE), # 60 a 69 - 18%
  sample(70:79,n * 0.16, replace = TRUE), # 70 a 79 - 16%
  sample(80:105, n * 0.14, replace = TRUE) # 80+ - 14%
)

idade <- factor(idade,   # transformando os numeros em labels
                levels = c(1:8),
                labels = c("2 a 9 anos",
                          "10 a 19 anos",
                          "20 a 29 anos",
                          "30 a 39 anos",
                          "40 a 49 anos",
                          "50 a 59 anos",
                          "60 a 69 anos",
                          "70+ anos")) 

renda <- rnorm(n, mean = 1524.63, sd = 500)
hist(renda)

trabalho <- rbinom(n,1,.29) # 0 eh empregado e 1 eh desempregado 

trabalho <- factor(trabalho,
                levels = c(0:1),
                labels = c("Empregado",
                          "Desempregado")) 

pagador <- rbinom(n,1,.85) # 0 eh mau e 1 eh bom

pagador <- factor(pagador,
                levels = c(0:1),
                labels = c("Mau Pagador",
                          "Bom Pagador")) 

genero <- rbinom(n,1,.63) # 0 eh homem e 1 eh mulher

genero <- factor(genero,
                levels = c(0:1),
                labels = c("Masculino",
                          "Feminino")) 

# Gráficos de Barra OU Linhas
library(ggplot2)

# comparar grupos de idade x qualidade pagador - linhas eh melhor pq sao vários
comp_idade_pagador <- data.frame(idade, pagador) 

# comparar genero x qualidade pagador - barra eh melhor pq sao 2 grupos

# empregabilidade x qualidade pagador - barra eh melhor pq sao 2 grupos

# comparar renda por faixas etárias (infatojuvenil, adulto, idosos) - correlação é uma boa, mas pd ser media de renda por faixa etária (barras)

# faixas etárias x qualidade pagador - barra ou linha btw