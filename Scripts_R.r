# Configurar Work Directory
setwd("C/Users/dmpm/Documents/PowerBI/Cap12")

# Obter caminho do Work Directory
getwd()

# Carregar o DataSet
vendas <- read.csv("Vendas.csv", fileEncoding = "windows-1252")

# Resumo do DataSet
View(Vendas) -- Visualização / impressão da tabela
str(vendas) -- Resumo completo de todas variáveis
sumary(vendas$Valor) -- Sumário da Coluna Valor
sumary(vendas$Custo)

# Média
?mean -- Imprime visualização da documentação da função
mean(vendas$Valor)
mean(vendas$Custo)

# Média Ponderada
?weighted.mean
weighted.mean(vendas$Valor, w = vendas$Custo)

# Mediana
median(vendas$Valor)
median(vendas$Custo)

# Moda
moda <- function(v) {
	valor_unico <- unique(v)
	valor_unico[which.max(tabulate(match(v, valor_unico)))]
}

resultado <- moda(vendas$Valor)
print(resultado)

unique - retorna os valores únicos do parâmetro passado
which.max - retorna a posição do elemento com maximo valor
tabulate - Função subjacente para uma tabela
match - retorna a vetor da poosição correspondente

# Criando Gráfico de Média de Valor Por Estado com ggplot2
install.packages("ggplot2")
library(ggplot2)

# Criando o grafico
ggplot(vendas) +
	stat_summary(aes( x = Estado, y = Valor), -- Camada de Sumarização
				 fun = mean,       -- Função Media
				 geom = "bar",     -- Grafico Barras
				 fill = "lightgreen", -- Cor das Colunas G. Barras
				 col = "grey50"       -- Cor Geral Grafico
				) +
	labs(title = "Média de Valor Por Estado") -- Titulo

# Medidas de Dispersão

# Calculando a Variancia
var(vendas$Valor)

# Calculando o Desvio Padrão
sd(vendas$Valor)

# Medidas de Posição Relativa

head(vendas) -- Cabeça/Primeiras linhas
tail(vendas) -- Traseira/Ultimas linhas
View(vendas) -- Visualização da Tabela

# Medidas de Tendência Central
sumary(vendas$Valor)
sumary(vendas[c'Valor', 'Custo') ]) -- Filtrando apenas duas colunas

#Explorando variávies númericas
mean(vendas$Valor) -- media
median(vendas$Valor) -- mediana
quantile(vendas$Valor) -- quartiz
quantile(vendas$Valor, probs = c(0.01, 0.99) ) -- Percentil de 2 posições
quantile(vendas$Valor, seq(from = 0, to = 1, by = 0.20) ) -- Percentil de Range - 0 à 1, saltando de 0.20 por vez
IQR(vendas$Valor) -- Diferença entre Q3 e Q1
range(vendas$Valor) -- Traz valor min e max da coluna
sumary(vendas$Valor)
diff(range(vendas$Valor) ) -- Diferença entre o valor min e max

# Carregando o DataSet
notas <- read.csv("Notas.csv", fileEncoding = "windows-1252")

# Apresentar um resumo de tipos de dados e estatisticas do dataset
View(notas)
str(notas)
summary(notas$TurmaA)
summary(notas$TurmaB)

# Média de cada turma
mean(notas$TurmaA)
mean(notas$TurmaB)

# Turma que apresentou maior variabilidade de notas
sd(notas$TurmaA)
sd(notas$TurmaB)

# Coeficiente de variação das 2 turmas
media_ta <- mean(notas$TurmaA)
media_tb <- mean(notas$TurmaB)

sd_ta <- sd(notas$TurmaA)
sd_tb <- sd(notas$TurmaB)

CVA <- sd_ta / media_ta * 100
CVB <- ds_tb / media_tb * 100

print(CVA)
print(CVB)

# Nota que apareceu mais vezes em cada turma
calculaModa <- function(v) {
	uniqv <- unique(v)
	uniqv[which.max(tabullate(match(v, uniqv)))]
}

calculaModa(notas$TurmaA)
calculaModa(notas$TurmaB)

# Tabela de Frequencia
# Carregando os Dados
dados <- read.table("Usuarios.csv",
		dec = ".",
		sep = ",",
		h   = T,
		fileEncoding = "windows-1252"
)

# Visualizando e Sumarizando os Dados
View(dados)
names(dados)
str(dados)
summary(dados$salario)
summary(dados$grau_instrucao)
mean(dados$salario)
mean(dados$grau_instrucao)

# Tabela de Frequencias Absolutas
freq <- table(dados$grau_instrucao)
View(freq)

# Tabela de Frequencias Relativas
freq_rel <- prop.table(freq)
View(freq_rel)

