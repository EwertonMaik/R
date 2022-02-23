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
?cut # Converte uma variavel numerica em categorica
dados_clientes$Genero <- cut(dados_clientes$Genero,
							 c(0,1,2),
							 labels = c("Masculino", "Feminino")
)
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)

# Escolaridade
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade,
								   c(0,1,2,3,4),
								   labels = c("Pos Graduado",
											  "Graduado",
											  "Ensino Medio",
											  "Outros")
)

View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

# Estado Civil
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil,
								   c(-1,0,1,2,3),
								   labels = c("Desconhecido",
											  "Casado",
											  "Solteiro",
											  "Outro")
)

View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

# Convertendo a variavel para o tipo fator com faixa etária
str(dados_clientes$Idade)
summary(dados_clientes$Idade)
hist(dados_clientes$Idade)
dados_clientes$Idade <- cut(dados_clientes$Idade,
							c(0, 30, 50, 100),
							labels = c("Jovem", "Adulto", "Idoso")
)

View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)

# Convertendo a variavel que indica pagamentos para o tipo fator
# Altera apenas o Tipo da Variavel, mantendo o valor
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)

# Dataset após as conversões
str(dados_clientes)
sapply(dados_clientes, function(x) sum(is.na(x) ) )
missmap(dados_clientes, main = "Valores Missing Observados")
dados_cliente <- na.omit(dados_clientes)
missmap(dados_clientes, main = "Valores Missing Observados")
dim(dados_clientes)
View(dados_clientes)

# Alterando a variavel dependente para o tipo fator
str(dados_clientes$inadimplente)
colnames(dados_clientes)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)
str(dados_clientes$inadimplente)
View(dados_clientes)

# Total de inadimplente versus não-inadimplente
table(dados_clientes$inadimplente)

# Vejamos as porcentagens entre as classes
prop.table(table(dados_clientes$inadimplente))

# Plot da distribuição usando ggplot2
ggplot(inadimplente, data = dados_clientes, geom = "bar") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set seed
set.seed(1234)

# Amostragem estratificada
?createDataPartition
indice <- createDataPartition(dados_clientes$inadimplente, p = 0.75, list = FALSE)
dim(indice)

# Definimos os dados de treinamento como subconjunto de dados original
# com numeros de indice de linha conforme identificado acima e todas as colunas
dados_treino <- dados_clientes[indice,]
dim(dados_treino)
table[dados_treino$inadimplente]

# Veja porcentagens entre as classes
prop.table(table(dados_treino$inadimplente))

# Numero de registros no dataset de treinamento
dim(dados_treino)

# Comparamos as porcentagens entre as classes de treinamento e dados originais
compara_dados <- cbind(prop.table(table(dados_treino$inadimplente) )
						prop.table(table(dados_clientes$inadimplente) ) )
colnames(compara_dados) <- c("Treinamento", "Original")
compara_dados

# Melt Data - Converte colunas em linhas
?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Plot para ver a distribuicao do treinamento vs original
ggplot(melt_compara_dados, aes(x = X1, y = value))
+ geom_bar( aes(fill = X2), stat = "identity", position = "dodge" )
+ theme(axis.text.x = element_text(angle = 90, hjust = 1) )

# Tudo o que nao esta no dataset de treinamento esta no dataset de teste. Observer o sinal
dados_teste <- dados_clientes[-indice,]
dim(dados_teste)
dim(dados_treino)