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
