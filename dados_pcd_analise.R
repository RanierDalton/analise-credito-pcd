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

proporcoes <- c(0.04, 0.05, 0.06, 0.08, 0.12, 0.17, 0.18, 0.16, 0.14)
grupos <- 1:9
idade_grupo <- sample(grupos, size = 900, replace = TRUE, prob = proporcoes)

idade_grupo

idade_grupo <- factor(idade_grupo,   # transformando os numeros em labels
                levels = c(1:9),
                labels = c("2 a 9 anos",
                          "10 a 19 anos",
                          "20 a 29 anos",
                          "30 a 39 anos",
                          "40 a 49 anos",
                          "50 a 59 anos",
                          "60 a 69 anos",
                          "70 a 79 anos",
                          "80+ anos"))
table(idade_grupo)

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
                labels = c("Bom Pagador",
                          "Mau Pagador")) 

genero <- rbinom(n,1,.63) # 0 eh homem e 1 eh mulher

genero <- factor(genero,
                levels = c(0:1),
                labels = c("Masculino",
                          "Feminino")) 

library(ggplot2)

comp_idade_pagador <- data.frame(idade_grupo, pagador) 
table(comp_idade_pagador)

library(dplyr)

df_resumo <- comp_idade_pagador %>%
  group_by(idade_grupo, pagador) %>%
  summarise(Quantidade = n(), .groups = "drop")
df_resumo

ggplot(df_resumo, aes(x = idade_grupo, y = Quantidade, fill = pagador)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribuição de Pagadores por Faixa Etária",
       x = "Faixa Etária",
       y = "Quantidade",
       fill = "Tipo de Pagador")

df <- data.frame(IdadeGrupo = idade_grupo, Trabalho = trabalho, Pagador = pagador, Genêro = genero, Renda = renda)
df
mediaInfantoJuvenil <- mean(c(mean(df$Renda[df$IdadeGrupo == "2 a 9 anos"]), mean(df$Renda[df$IdadeGrupo == "10 a 19 anos"])))
mediaInfantoJuvenil

mediaAdulto <- mean(c(mean(df$Renda[df$IdadeGrupo == "20 a 29 anos"]), mean(df$Renda[df$IdadeGrupo == "30 a 39 anos"]), mean(df$Renda[df$IdadeGrupo == "40 a 49 anos"]), mean(df$Renda[df$IdadeGrupo == "50 a 59 anos"])))
mediaAdulto

mediaIdosos <- mean(c(mean(df$Renda[df$IdadeGrupo == "60 a 69 anos"]), mean(df$Renda[df$IdadeGrupo == "70 a 79 anos"]), mean(df$Renda[df$IdadeGrupo == "80+ anos"])))
mediaIdosos

medias <- data.frame(FaixaEtária = c("Infantojuvenil", "Adulto", "Idosos"), Médias = c(mediaInfantoJuvenil, mediaAdulto, mediaIdosos))

ggplot(medias, aes(x = FaixaEtária, y = Médias)) +
  geom_bar(stat = "identity", color = "black", fill = c("blue4", "darkgreen", "red4")) +
  labs(title = "Média de renda por faixa etária", x = "Faixas Etária", y = "Médias")
