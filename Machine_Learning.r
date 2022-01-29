#

# Definindo a pasta de trabalho
setwd("C:/Users/dmpm/Documents/PowerBI/Cap15")
getwd()

# Instalando os pacotes para projeto
# Obs: os pacotes precisam ser instalados apenas uma vez
install.packages("Amelia")
install.packages("caret")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape")
install.packages("randomForest")
install.packages("e1071")

# Carregando os Pacotes
library("Amelia")
library("caret")
library("ggplot2")
library("dplyr")
library("reshape")
library("randomForest")
library("e1071")

# Carregando o dataset
# Fonte: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients
dados_clientes <- read.csv("dados/dataset.csv")

# Visualizando os dados e sua estrutura
View(dados_clientes)
dim(dados_clientes)
str(dados_clientes)
summary(dados_clientes)

# Análise Exploratória, Limpeza e Transformação

# Removendo a primeira coluna ID
dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

# Removendo a coluna de classe
colnames(dados_clientes)
colnames(dados_clientes)[24] <- "inadimplente"
colnames(dados_clientes)
View(dados_clientes)

# Verificando valores ausentes e removendo do dataset
sapply(dados_clientes, function(x) sum(is.na(x) ) )
?missmap
missmap(dados_clientes, main = "Valores Missing Observados")
dados_clientes <- na.omit(dados_clientes)

# Convertendo os atributos genero, escolaridade, estado civil e idade
# para fatores (categorias)
str(dados_clientes)

# Renomeando colunas categoricas
colnames(dados_clientes)
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[2] <- "Escolaridade"
colnames(dados_clientes)[2] <- "Estado_Civil"
colnames(dados_clientes)[2] <- "Idade"
colnames(dados_clientes)
View(dados_clientes)

# Genero
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)
?cut
dados_clientes$Genero <- cut(dados_clientes$Genero,
							 c(0,1,2),
							 labels = c("Masculino", "Feminino")
)
View(dados_clientes$Genero)
str(dados_clientes$Genero)