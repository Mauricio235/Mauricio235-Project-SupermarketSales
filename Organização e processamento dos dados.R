library(ggplot2)
library(dplyr)
library(tidyr)
library(ggExtra)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(viridisLite)
library(RColorBrewer)

setwd("C:\\Users\\mauri\\Desktop\\PROJETO OPERDATA\\Projeto")
dados<-read.csv("BASE DE DADOS1-formatada.csv", header = T, sep = ",", dec = ';')

colnames(dados)
dados$X<-NULL

colnames(dados)<-c( "ID", "Filial", "Cidade", "Tipo de consumidor",
                    "Gênero", "Linha de produto", "Preço Unitário", "Quantidade",
                    "Imposto", "Total", "Data", "Hora",
                   "Pagamento", "Porcentagem de renda",
                    "Avaliação")


head(dados$Total)

dados$total<-NULL
dados$`Preço Unitário`<-dados$`Preço Unitário`*5.31
dados$Imposto<-dados$Imposto*5.31
dados$Total<-(dados$`Preço Unitário`*dados$Quantidade)+dados$Imposto
dados$`Porcentagem bruta`<-NULL
dados$engrenagens<-NULL

head(dados$total)



#Cotação do dólar

dados$`Valor esperado`<-(dados$`Valor esperado`)*5.31 
dados$`Valor planejado`<-(dados$`Valor planejado`)*5.31
dados$`Real valor arrecadado`<-(dados$`Real valor arrecadado`)*5.31

dados<-na.omit(dados)

#Formatando genero
dados$Gênero[dados$Gênero == "Male"] <- "Masculino"
dados$Gênero[dados$Gênero == "Female"] <- "Feminino"


#formatando tipo de consumidor
dados$`Tipo de consumidor`[dados$`Tipo de consumidor`== "Member"] <- "Membro"


#formatando linha de produto
unique(dados$`Linha de produto`)
dados$`Linha de produto`[dados$`Linha de produto`=="Health and beauty"]<-"Saúde e beleza"
dados$`Linha de produto`[dados$`Linha de produto`== "Electronic accessories"]<-"Acessórios eletrônicos"
dados$`Linha de produto`[dados$`Linha de produto`=="Lar e estado de vida"]<-"Lar e estilo de vida"
dados$`Linha de produto`[dados$`Linha de produto`==  "Sports and travel" ]<-"Viagens e sports"
dados$`Linha de produto`[dados$`Linha de produto`== "Food and beverages"]<-"Alimentos e bebidas"
dados$`Linha de produto`[dados$`Linha de produto`==  "Fashion accessories" ]<-"Acessórios de moda"

unique(dados$`Categoria`)


summary(dados$Quantidade)

#Formatando forma de pagamento
dados$Pagamento[dados$Pagamento=="Cash"]<-"Dinheiro"
dados$Pagamento[dados$Pagamento=="Credit card"]<-"Cartão de crédito"
dados$Pagamento[dados$Pagamento=="Ewallet"]<-"Carteira eletrônica"





write.csv(dados, file="BASE DE DADOS1.csv", col.names = T)

# Plot
dados %>%
  ggplot( aes(x=Filial, y=Quantidade, fill=Filial)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Quantidade por filial") +
  xlab("")



#Histograma para avaliação
as.numeric(dados$Avaliação)
hist(dados$Quantidade, breaks=30, xlim=c(0,10), xlab="Avaliações", col=rgb(1,0,0,0.5),
     ylab="Score", main="Avaliações dos clientes" )



write.csv(dados, file="BASE DE DADOS1.csv", col.names = T)





