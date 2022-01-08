# Limpeza e Transformação de Dados com Power Query e Linguagem R

# Definindo pasta de trabalho
setwd("C:/Users/dmpm/Documentos/PowerBI/Cap12")
getwd()

# Instalaçao de pacotes
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")

# Carregando pacotes
library(dplyr)
library(data.table)
library(ggplot2)

# Carregando Fonte de Dados - https://archive.ics.uci.edu/ml/datasets/iris
dados_iris <- iris
View(dados_iris)

# Atividade 1 - Sumarizar os dados com as medias de cada coluna de um dataset
library(dplyr)
medias_iris <- summarize(group_by(dados_iris, Species),
						 media_sepal_length = mean(Sepal.Length),
						 media_sepal_width  = mean(Sepal.Width),
						 media_petal_length = mean(Petal.Length),
						 media_petal_width  = mean(Petal.Width)
)

View(media_iris)