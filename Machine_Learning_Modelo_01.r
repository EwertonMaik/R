## Modelo 01 de Machine Learning ##

# Construindo a primeira versão do Modelo
# randomForest - Arvore de decisão - classificação e regressão com randomForest
?randomForest
view(dados_treino)
modelo_v1 <- randomForest(inadimplente ~ ., data = dados_treino)
modelo_v1

# Avaliando o Modelo
plot(modelo_v1)

# Previsoes com dados de teste
previsoes_v1 <- predict(modelo_v1, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v1 <- caret::confusionMatrix(previsoes_01, dados_teste$inadimplente, positive = "1" )
cm_v1

# Calculando Precision, Recall e F1-Score, metricas de avaliação de modelo preditivo
y <- dados_teste$inadimplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)

# Balanceamento de classe
install.packages("DMwR")
library(DMwR)
?SMOTE

# Aplicando o SMOTE - SMOTE : Synthetic Minority Over - Sampling Technique
# https://arxiv.org/pdf/1106.1813.pdf

table(dados_treino$inadimplente)
prop.table(table(dados_treino$inadimplente))
set.seed(9560)
dados_treino_bal <- SMOTE(inadimplente ~ ., data = dados_treino)
table(dados_treino_bal$inadimplente)
prop.table(table(dados_treino_bal$inadimplente) )

# Costruindo a segunda versão do modelo
modelo_v2 <- randomForest(inadimplente ~ ., data = dados_treino_bal)
modelo_v2

# Avaliando o Modelo
plot(modelo_v2)

# Previsoes com dados de Teste
previsoes_v2 <- predict(modelo_v2, dados_teste)

# Confusion Matrix
?caret::confusionMatrix
cm_v2 <- caret::confunsionMatrix(previsoes_v2, dados_teste$inadimplente, positive = "1")
cm_v2

# Calculando Precision, Recall e F1-Score, metricas de avaliação do modelo preditivo
y <- dados_teste$inadimplente
Y_pred_2 <- previsoes_v2

precision <- posPredValue(y_pred_2, y)
precision

recall <- sensitivity(y_pred_2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1


# Importancia das variaveis preditoras para as previsoes
View(dados_treino_bal)
varImPlot(modelo_v2)

# Obtendo as variaveis mais importantes
imp_var <- importance(modelo_v2)
varImportance <- data.frame(Variables = row.name(imp_var),
							Importance = round(imp_var[ , 'MeanDecreaseGini'], 2 ))