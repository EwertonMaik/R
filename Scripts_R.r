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

# Porcentagem ( 100 * freq_rel_table)
p_freq_rel <- 100 * prop.table(freq_rel)
View(p_freq_rel)

# Adiciona linha de total e aplica nome a posição 4
View(freq)
freq <- c(freq, sum(freq) )
names(freq)[4] <- "Total"
View(freq)

# Tabela final com todos os valores
# Calculando frequencia relativa e frequencia proporcional

freq_rel   <- c(freq_rel, sum(freq_rel) )
p_freq_rel <- c(p_freq_rel, sum(p_freq_rel) )

# Tabela final com todos os vetores
tabela_final <- cbind(
	freq,
	freq_rel   = round(freq_rel, digits = 2),
	p_freq_rel = round(p_freq_rel, digits = 2)
)

View(tabela_final)

# Criação de Demais Gráficos com R
# Configurar Work Directory
setwd("C/Users/dmpm/Documents/PowerBI/Cap12")

# Obter caminho do Work Directory
getwd()

# Dados - Criando na mesma variável 2 verotes
vetor_total_resultados = c(3, 12, 5, 18, 45)
names(vetor_total_resultados) = c("A", "B", "C", "D", "E")
vetor_total_resultados

# Funcao BARPLOT -- Grafico de Barras
barplot(vetor_total_resultados)
barplot(vetor_total_resultados, col = c(1, 2, 3, 4, 5) ) # Personalizo o Grafico, cada barra com uma com, conforme vetor passado para o parâmetro COL

# Salvar a imagem do Grafico anterior no disco
png("barplot.png", width = 480, height = 480) # Criando arquivo vazio
barplot(vetor_total_resultados,
			col  = rgb(0.5, 0.1, 0.6, 0.6),
			xlab = "Categorias",
			ylab = "Valores",
			main = "Barplot em R",
			ylim = c(0,60)
		)
dev.off() # Salvando em Disco

# Ggplot2
library(ggplot2) # Carregando Biblioteca
Views(mtcars) # Visualizando DataSet

# Barplot -- Criando Grafico de Barras -- Gramatica dos Graficos
ggplot(mtcars, aes(x = as.factor(cyl) ) ) + geom_bar()

# Mesmo Grafico Customizando as cores
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl) ) )
+ geom_bar()
+ scale_fill_manual(values = c("red", "green", "blue") )

# Criando outro exemplo dados Dummy (Ficticios)
dados = data.frame(group = c("A", "B", "C", "D"), value = c(33, 62, 56, 67) )
View(dados)

# Barplot
ggplot(dados, aes(x = group, y = value, fill = group) )
+ geom_bar(width = 0.85, stat = "identity")

# Pie Chart - Grafico de Pizza
fatias <- c(4, 12, 14, 16, 8)
paises <- c("Brasil", "Estados Unidos", "Alemanha", "Reino Unido", "Espanha")
pie(fatias, labels = paises, main = "Leitura de Livros Por Pessoa/Ano")

# pie Chart 3D - Grafico de Pizza 3D
install.packages("plotrix")
library(plotrix)

fatias <- c(4, 12, 14, 16, 8)
paises <- c("Brasil", "Estados Unidos", "Alemanha", "Reino Unido", "Espanha")
pie3D(fatias, labels = paises, explode = 0.1, main = "Leitura de Livros Por Pessoa/Ano")

### Line Chart ###

# Dados
carros     <- c(1, 3, 6, 4, 9)
caminhoes  <- c(2, 5, 4, 5, 12)

# Plot - Criando Grafico de Linhas
plot(carros, type = "o", col = "blue", ylim = c(0, 12) ) # Primeira Camada

lines(caminhoes, type = "o", pch = 22, lty = 2, col = "red") # Segunda Camada

title(main = "Produção de Veiculos", col.main = "red", front.main = 4) # Terceira Camada

### Boxplot ###
library(ggplot2) #Carregando biblioteca em memória
View(mpg) # Visualizando Conjunto de Dados - dataset

# Plot
ggplot(mpg, aes(x = reorder(class, hwy), y = hwy, fill = class ) )
+ geom_boxplot()
+ xlab("class")
+ theme(legend.position = "none")

### Scatter Plot ###
library(ggplot2)
data = data.frame(cond = rep(c("condition_1", "condition_2"), each = 10 ),
				  my_x = 1:100 + rnorm(100, sd = 9), my_y = 1:100 + rnorm(100, sd = 16) )
View(data)

ggplot(data, aes(x = my_x, y = my_y) ) + geom_point(shape = 1) # Gafico de Dispersão

# Adicionando linha de regressão ao Grafico anterior
ggplot(data, aes(x = my_x, y = my_y) )
+ geom_point(shape = 1)
+ geom_smooth(method = lm, color = "red", se = FALSE)

# Adicionando Smooth
ggplot(data, aes(x = my_x, y = my_y) )
+ geom_point(shape = 1)
+ geom_smooth(method = lm, color = "red", se = TRUE)

### TreeMap ###
install.packages("treemap")
library(treemap)

# Dados
grupo    = c(rep("grupo-1", 4), rep("grupo-2", 2), rep("grupo-3", 3) )
subgrupo = paste("subgroup", c(1,2,3,4,1,2,1,2,3), sep = "-" )
vaor     = c(13, 5, 22, 12, 11, 7, 3, 1, 23)
dados = data.frame(grupo, subgrupo, valor)
View(dados)

# Labels
?treemap
treemap(dados,
		index = c("grupo", "subgrupo"),
		vSize = "valor",
		type  = "index",
		fontsize.labels  = c(15, 12),
		fontcolor.labels = c("white", "orange"),
		fontface.labels  = c(2, 1),
		bg.labels = 220,
		align.labels = list( c("center", "center"), c("right", "bottom") ),
		overlap.labels = 0.5,
		inflate.labels = F
)

# Invertendo e Customizando TreeMap
treemap(dados,
		index  = c("grupo", "subgrupo"),
		vSize  = "valor",
		type   = "index",
		border.col  = c("black", "white"),
		border.lwds = c(7, 2)
)

### Histogramma ###
# Gerando valores para X
x <- mtcars$mpg # Pegando uma única coluna da tabela

# Criando o Histograma
h <- hist(x,
		  breaks = 10,
		  col    = "red",
		  xlab   = "Milhas Por Galão",
		  main   = "Histograma com Curva de Distribuição"
)

# Customizando o Histograma
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x) )
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)

# Usando o ggplot2
library(ggplot2)

# Dataset
dados = data.frame(value = rnorm(10000) )
View(dados)

# Tamanho das colunas
ggplot(dados, aes(x = value))
+ geom_histogram(binwidth = 0.05)

# Cor Uniforme
ggplot(dados, aes(x = value))
+ geom_histogram(binwidth = 0.2, color = "white", fill = rgb(0.2, 0.7, 0.1, 0.4) )

# Cor Proporcional
ggplot(dados, aes(x = value) )
+ geom_histogram(binwidth = 0.2, aes(fill = ..count..) )