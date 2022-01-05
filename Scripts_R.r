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