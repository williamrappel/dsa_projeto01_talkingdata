# Diretório de trabalho
setwd("C:/FCD/BigDataRAzure/Cap20/Projeto01")
getwd()

# Pacotes de manipulação
library(data.table)
library(dplyr)
library(lubridate)

# Leitura do banco de dados completo
system.time(df <- fread("train.csv"))

# Estrutura
str(df)
head(df)

# Variável resposta
(tab <- table(df$is_attributed))
prop.table(tab)

# Rebalanceamento
set.seed(100)
ind_zero <- which(df$is_attributed == 0)
ind_zero_novo <- sample(ind_zero, tab[2])
df2 <- df[c(ind_zero_novo, which(df$is_attributed == 1)),]
rm(df, ind_zero, ind_zero_novo)

# Novo banco de dados
str(df2)
head(df2)
table(df2$is_attributed)
sapply(df2, function(x) length(unique(x)))

# Removendo variáveis
df2 <- df2 %>%
  select(-ip, -attributed_time)
saveRDS(df2, file = "df2.rds")
df2 <- readRDS("df2.rds")

# Reorganizando variáveis
# app
(tab_app <- sort(table(df2$app), decreasing = T))
app_mant <- as.numeric(names(tab_app)[tab_app >= 10000])
app_mod <- ifelse(df2$app %in% app_mant, df2$app, 999)
table(app_mod)
df2$app_mod <- app_mod
# app_mant: 1, 2, 3, 5, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 29, 35, 45, 72

# device
(tab_dev <- sort(table(df2$device), decreasing = T))
dev_mant <- as.numeric(names(tab_dev)[tab_dev >= 1000])
dev_mod <- ifelse(df2$device %in% dev_mant, df2$dev, 999)
table(dev_mod)
df2$device_mod <- dev_mod
# dev_mant: 0, 1, 2, 6, 16, 18, 21, 33, 40, 3032

# os
(tab_os <- sort(table(df2$os), decreasing = T))
os_mant <- as.numeric(names(tab_os)[tab_os >= 10000])
os_mod <- ifelse(df2$os %in% os_mant, df2$os, 999)
table(os_mod)
df2$os_mod <- os_mod
# os_mant: 0, 6, 8, 9, 10, 13, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 27, 29, 32, 37, 38

# channel
(tab_cha <- sort(table(df2$channel), decreasing = T))
cha_mant <- as.numeric(names(tab_cha))[tab_cha >= 10000]
cha_mod <- ifelse(df2$channel %in% cha_mant, df2$channel, 999)
table(cha_mod)
df2$channel_mod <- cha_mod
# cha_mant: 21, 101, 107, 113, 121, 134, 145, 153, 178, 205, 213, 245, 259, 265, 274, 280, 343, 347, 442, 477

# clicktime
df2$click_time <- ymd_hms(df2$click_time)
df2 <- df2 %>%
  mutate(weekday = as.factor(weekdays(click_time)), hour = hour(click_time))
str(df2)
df2$weekday <- as.numeric(ordered(df2$weekday, levels = c("segunda-feira",
                                                          "terça-feira",
                                                          "quarta-feira",
                                                          "quinta-feira")))

final <- df2 %>%
  select(app_mod, device_mod, os_mod, channel_mod, weekday, hour, is_attributed) %>%
  mutate_at(c("app_mod", "device_mod", "os_mod", "channel_mod"), as.factor)
str(final)
saveRDS(final, file = "final.rds")
final <- readRDS("final.rds")

# Treino e Validação
set.seed(200)
treino <- sample(1:nrow(final), ceiling(0.7*nrow(final)))
df_treino <- final[treino,]
df_teste <- final[-treino,]

str(df_treino)
str(df_teste)
prop.table(table(df_treino$is_attributed))
prop.table(table(df_teste$is_attributed))
sapply(final, function(x) length(unique(x)))
sapply(df_treino, function(x) length(unique(x)))
sapply(df_teste, function(x) length(unique(x)))
rm(list=ls()[!(ls() %in% c('df_treino','df_teste'))])
saveRDS(df_treino, file = "df_treino.rds")
df_treino <- readRDS("df_treino.rds")
saveRDS(df_teste, file = "df_teste.rds")
df_teste <- readRDS("df_teste.rds")

# Análise Exploratória
# NA
sapply(df_treino, function(x) sum(is.na(x)))
sapply(df_teste, function(x) sum(is.na(x)))
library(tigerstats)
library(ggplot2)
# Individual
# app_mod
rowPerc(xtabs(~ app_mod, data = df_treino))
ggplot(df_treino, aes(x = app_mod)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") + 
  labs(x="App (modificado)", y="Porcentagem") +
  scale_y_continuous(breaks = 0:15, labels = paste0(0:15, '%'))
# device_mod
rowPerc(xtabs(~ device_mod, data = df_treino))
ggplot(df_treino, aes(x = device_mod)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") + 
  labs(x="Device (modificado)", y="Porcentagem") +
  scale_y_continuous(breaks = seq(0,100,10), labels = paste0(seq(0,100,10), '%'), limits = c(0,100))
# os_mod
rowPerc(xtabs(~ os_mod, data = df_treino))
ggplot(df_treino, aes(x = os_mod)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") + 
  labs(x="OS (modificado)", y="Porcentagem") +
  scale_y_continuous(breaks = 0:25, labels = paste0(0:25, '%'), limits = c(0,25))
# channel_mod
rowPerc(xtabs(~ channel_mod, data = df_treino))
ggplot(df_treino, aes(x = channel_mod)) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21") + 
  labs(x="Channel (modificado)", y="Porcentagem") +
  scale_y_continuous(breaks = seq(0,40,5), labels = paste0(seq(0,40,5), '%'), limits = c(0,40))
# weekday
rowPerc(xtabs(~ weekday, data = df_treino))
ggplot(df_treino, aes(x = as.factor(weekday))) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21", width = .5) + 
  labs(x="Weekday", y="Porcentagem") +
  scale_y_continuous(breaks = seq(0,35,5), labels = paste0(seq(0,35,5), '%'), limits = c(0,35)) +
  scale_x_discrete(labels = c("segunda-feira", "terça-feira", "quarta-feira", "quinta-feira"))
# hour
rowPerc(xtabs(~ hour, data = df_treino))
ggplot(df_treino, aes(x=hour)) + geom_histogram(aes(y = 100 * (..count..)/sum(..count..)), 
                                                colour="white", fill="#A11D21", binwidth=1, center = .5)+
  labs(x="Hour", y="Porcentagem") + 
  scale_y_continuous(breaks = 0:11, labels = paste0(0:11, '%'), limits = c(0,11))
# is_attributed
rowPerc(xtabs(~ is_attributed, data = df_treino))
ggplot(df_treino, aes(x = as.factor(is_attributed))) +
  geom_bar(aes(y = prop.table(..count..) * 100), fill = "#A11D21", width = .3) + 
  labs(x="Is_attributed", y="Porcentagem") +
  scale_y_continuous(breaks = seq(0,50,10), labels = paste0(seq(0,50,10), '%'), limits = c(0,55))
# Bivariada
library(scales)
# app_mod
rowPerc(xtabs(~ app_mod + is_attributed, data = df_treino))
tab <- as.data.frame(prop.table(table(df_treino$app_mod,df_treino$is_attributed),1))
ggplot(tab, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat = "identity", position="fill") +
  scale_fill_manual(values=c("#A11D21", "#003366"), name = "Is_attributed")+
  labs(x="App (modificado)", y="Porcentagem") +
  theme(legend.position="top") +
  scale_y_continuous(labels = scales::percent)
# device_mod
rowPerc(xtabs(~ device_mod + is_attributed, data = df_treino))
tab <- as.data.frame(prop.table(table(df_treino$device_mod,df_treino$is_attributed),1))
ggplot(tab, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat = "identity", position="fill") +
  scale_fill_manual(values=c("#A11D21", "#003366"), name = "Is_attributed")+
  labs(x="Device (modificado)", y="Porcentagem") +
  theme(legend.position="top") +
  scale_y_continuous(labels = scales::percent)
# os_mod
rowPerc(xtabs(~ os_mod + is_attributed, data = df_treino))
tab <- as.data.frame(prop.table(table(df_treino$os_mod,df_treino$is_attributed),1))
ggplot(tab, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat = "identity", position="fill") +
  scale_fill_manual(values=c("#A11D21", "#003366"), name = "Is_attributed")+
  labs(x="OS (modificado)", y="Porcentagem") +
  theme(legend.position="top") +
  scale_y_continuous(labels = scales::percent)
# channel_mod
rowPerc(xtabs(~ channel_mod + is_attributed, data = df_treino))
tab <- as.data.frame(prop.table(table(df_treino$channel_mod,df_treino$is_attributed),1))
ggplot(tab, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat = "identity", position="fill") +
  scale_fill_manual(values=c("#A11D21", "#003366"), name = "Is_attributed")+
  labs(x="Channel (modificado)", y="Porcentagem") +
  theme(legend.position="top") +
  scale_y_continuous(labels = scales::percent)
# weekday
rowPerc(xtabs(~ weekday + is_attributed, data = df_treino))
tab <- as.data.frame(prop.table(table(df_treino$weekday,df_treino$is_attributed),1))
ggplot(tab, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat = "identity", position="fill", width = .5) +
  scale_fill_manual(values=c("#A11D21", "#003366"), name = "Is_attributed")+
  labs(x="Weekday", y="Porcentagem") +
  theme(legend.position="top") +
  scale_y_continuous(labels = scales::percent)
# hour
rowPerc(xtabs(~ hour + is_attributed, data = df_treino))
tab <- as.data.frame(prop.table(table(df_treino$hour,df_treino$is_attributed),1))
ggplot(tab, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat = "identity", position="fill", width = .5) +
  scale_fill_manual(values=c("#A11D21", "#003366"), name = "Is_attributed")+
  labs(x="Hour", y="Porcentagem") +
  theme(legend.position="top") +
  scale_y_continuous(labels = scales::percent)
# Correlation
cor(df_treino[,c("is_attributed", "weekday", "hour")])

# Pacotes de modelos
library(caret)
library(ROCR)
library(pROC)
library(randomForest)
library(class)
library(gbm)
source("plot_utils.R")

# Regressão Logística
# Completo
reg.log <- glm(is_attributed ~ app_mod + device_mod + os_mod + channel_mod + weekday + hour,
               data = df_treino, family = binomial(link = logit))
saveRDS(reg.log, "reg.log.rds")
reg.log <- readRDS("reg.log.rds")
reg.log.probs <- predict(reg.log, newdata = df_teste, type = "response")
reg.log.preds <- round(reg.log.probs)
confusionMatrix(table(observado = df_teste$is_attributed, predito = reg.log.preds), positive = "1")
predictions <- prediction(reg.log.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, reg.log.probs))

# Random Forest
set.seed(300)
rf.mod <- randomForest(as.factor(is_attributed) ~ ., data = df_treino, importance = TRUE, 
                       subset = sample(1:nrow(df_treino), 100000))
saveRDS(rf.mod, "rf.mod.rds")
rf.mod <- readRDS("rf.mod.rds")
rf.mod
varImpPlot(rf.mod)
rf.probs <- predict(rf.mod, newdata = df_teste, type = "prob")[,2]
rf.preds <- predict(rf.mod, newdata = df_teste, type = "response")
confusionMatrix(table(observado = df_teste$is_attributed, predito = rf.preds), positive = "1")
predictions <- prediction(rf.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, rf.probs))

# KNN
set.seed(400)
ind <- sample(1:nrow(df_treino), 100000)
knn.preds <- knn(train = df_treino[ind,-c('is_attributed')], 
                 test = df_teste[,-c('is_attributed')],
                 cl = as.factor(df_treino$is_attributed[ind]), k = 50, prob = TRUE)
saveRDS(knn.preds, "knn.preds.rds")
knn.preds <- readRDS("knn.preds.rds")
knn.probs <- ifelse(knn.preds == 1, attributes(knn.preds)$prob, 1-attributes(knn.preds)$prob)
confusionMatrix(table(observado = df_teste$is_attributed, predito = knn.preds), positive = "1")
predictions <- prediction(knn.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, knn.probs))

# Bagging
set.seed(500)
bag.mod <- randomForest(as.factor(is_attributed) ~ ., data = df_treino, importance = TRUE, 
                        subset = sample(1:nrow(df_treino), 100000), mtry = 6)
saveRDS(bag.mod, "bag.mod.rds")
bag.mod <- readRDS("bag.mod.rds")
bag.mod
varImpPlot(bag.mod)
bag.probs <- predict(bag.mod, newdata = df_teste, type = "prob")[,2]
bag.preds <- predict(bag.mod, newdata = df_teste, type = "response")
confusionMatrix(table(observado = df_teste$is_attributed, predito = bag.preds), positive = "1")
predictions <- prediction(bag.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, bag.probs))

# Boosting
set.seed(600)
boost.mod <- gbm(is_attributed ~ ., data = df_treino, 
                 distribution = "bernoulli", n.trees = 100)
saveRDS(boost.mod, "boost.mod.rds")
boost.mod <- readRDS("boost.mod.rds")
boost.mod
summary(boost.mod)
boost.probs <- predict(boost.mod, newdata = df_teste, n.trees = 100, type = "response")
boost.preds <- round(boost.probs)
confusionMatrix(table(observado = df_teste$is_attributed, predito = boost.preds), positive = "1")
predictions <- prediction(boost.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, boost.probs))

# Otimização Regressão Logística
# Sem weekday
reg.log2 <- glm(is_attributed ~ app_mod + device_mod + os_mod + channel_mod + hour,
                data = df_treino, family = binomial(link = logit))
saveRDS(reg.log2, "reg.log2.rds")
reg.log2 <- readRDS("reg.log2.rds")
reg.log2.probs <- predict(reg.log2, newdata = df_teste, type = "response")
reg.log2.preds <- round(reg.log2.probs)
confusionMatrix(table(observado = df_teste$is_attributed, predito = reg.log2.preds), positive = "1")
predictions <- prediction(reg.log2.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, reg.log2.probs))
# Sem weekday e hour como fator
reg.log3 <- glm(is_attributed ~ app_mod + device_mod + os_mod + channel_mod + as.factor(hour),
                data = df_treino, family = binomial(link = logit))
saveRDS(reg.log3, "reg.log3.rds")
reg.log3 <- readRDS("reg.log3.rds")
reg.log3.probs <- predict(reg.log3, newdata = df_teste, type = "response")
reg.log3.preds <- round(reg.log3.probs)
confusionMatrix(table(observado = df_teste$is_attributed, predito = reg.log3.preds), positive = "1")
predictions <- prediction(reg.log3.probs, df_teste$is_attributed)
plot.roc.curve(predictions, title.text = "Curva ROC")
auc(roc(df_teste$is_attributed, reg.log3.probs))