# Diretório de trabalho
setwd("C:/FCD/BigDataRAzure/Cap20/Projeto01")
getwd()

# Pacotes
library(data.table)
library(dplyr)
library(lubridate)

# Vetores do Encoder
app_mant <- c(1, 2, 3, 5, 8, 9, 10, 11, 12, 13, 14, 15, 18, 19, 29, 35, 45, 72)
dev_mant <- c(0, 1, 2, 6, 16, 18, 21, 33, 40, 3032)
os_mant <- c(0, 6, 8, 9, 10, 13, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 27, 29, 32, 37, 38)
cha_mant <- c(21, 101, 107, 113, 121, 134, 145, 153, 178, 205, 213, 245, 259, 265, 274, 280, 343, 347, 442, 477)

# Leitura dos dados de teste
system.time(df <- fread("test.csv"))

# Manipulação
app_mod <- ifelse(df$app %in% app_mant, df$app, 999)
df$app_mod <- app_mod
dev_mod <- ifelse(df$device %in% dev_mant, df$dev, 999)
df$device_mod <- dev_mod
os_mod <- ifelse(df$os %in% os_mant, df$os, 999)
df$os_mod <- os_mod
cha_mod <- ifelse(df$channel %in% cha_mant, df$channel, 999)
df$channel_mod <- cha_mod
df$click_time <- ymd_hms(df$click_time)
df <- df %>%
  mutate(weekday = as.factor("segunda-feira"), hour = hour(click_time))
str(df)
df$weekday <- as.numeric(ordered(df$weekday, levels = c("segunda-feira",
                                                          "terça-feira",
                                                          "quarta-feira",
                                                          "quinta-feira")))
teste_final <- df %>%
  select(click_id, app_mod, device_mod, os_mod, channel_mod, weekday, hour)
teste_final$app_mod <- factor(teste_final$app_mod, levels = as.character(c(app_mant,999)))
teste_final$device_mod <- factor(teste_final$device_mod, levels = as.character(c(dev_mant,999)))
teste_final$os_mod <- factor(teste_final$os_mod, levels = as.character(c(os_mant,999)))
teste_final$channel_mod <- factor(teste_final$channel_mod, levels = as.character(c(cha_mant,999)))
str(teste_final)
rm(list=ls()[!(ls() %in% c('teste_final'))])
saveRDS(teste_final, file = "teste_final.rds")
teste_final <- readRDS("teste_final.rds")

# Pacotes de modelos
library(gbm)
library(randomForest)
library(class)

# Regressão Logística
# Completo
reg.log <- readRDS("reg.log.rds")
teste_final1 <- teste_final[1:5000000,]
reg.log.probs1 <- predict(reg.log, newdata = teste_final1, type = "response")
teste_final2 <- teste_final[5000001:10000000,]
reg.log.probs2 <- predict(reg.log, newdata = teste_final2, type = "response")
teste_final3 <- teste_final[10000001:18790469,]
reg.log.probs3 <- predict(reg.log, newdata = teste_final3, type = "response")
df.reg.log <- data.frame(click_id = teste_final$click_id, is_attributed = c(reg.log.probs1,reg.log.probs2,reg.log.probs3))
write.csv(df.reg.log, file = "reg_log.csv", row.names = FALSE)

# Boosting
boost.mod <- readRDS("boost.mod.rds")
boost.probs <- predict(boost.mod, newdata = teste_final, n.trees = 100, type = "response")
df.boost <- data.frame(click_id = teste_final$click_id, is_attributed = boost.probs)
write.csv(df.boost, file = "boost.csv", row.names = FALSE)

# Bagging
bag.mod <- readRDS("bag.mod.rds")
bag.probs <- predict(bag.mod, newdata = teste_final, type = "prob")[,2]
df.bag <- data.frame(click_id = teste_final$click_id, is_attributed = bag.probs)
write.csv(df.bag, file = "bag.csv", row.names = FALSE)

# Random Forest
rf.mod <- readRDS("rf.mod.rds")
rf.probs <- predict(rf.mod, newdata = teste_final, type = "prob")[,2]
df.rf <- data.frame(click_id = teste_final$click_id, is_attributed = rf.probs)
write.csv(df.rf, file = "rf.csv", row.names = FALSE)

# KNN
df_treino <- readRDS("df_treino.rds")
set.seed(400)
ind <- sample(1:nrow(df_treino), 100000)
knn.preds <- knn(train = df_treino[ind,-c('is_attributed')], 
               test = teste_final[,-c('click_id')],
               cl = as.factor(df_treino$is_attributed[ind]), k = 50, prob = TRUE)
knn.probs <- ifelse(knn.preds == 1, attributes(knn.preds)$prob, 1-attributes(knn.preds)$prob)
df.knn <- data.frame(click_id = teste_final$click_id, is_attributed = knn.probs)
write.csv(df.knn, file = "knn.csv", row.names = FALSE)

# Otimização Regressão Logística
# Sem weekday
reg.log2 <- readRDS("reg.log2.rds")
teste_final1 <- teste_final[1:5000000,]
reg.log2.probs1 <- predict(reg.log2, newdata = teste_final1, type = "response")
teste_final2 <- teste_final[5000001:10000000,]
reg.log2.probs2 <- predict(reg.log2, newdata = teste_final2, type = "response")
teste_final3 <- teste_final[10000001:18790469,]
reg.log2.probs3 <- predict(reg.log2, newdata = teste_final3, type = "response")
df.reg.log2 <- data.frame(click_id = teste_final$click_id, is_attributed = c(reg.log2.probs1,reg.log2.probs2,reg.log2.probs3))
write.csv(df.reg.log2, file = "reg_log2.csv", row.names = FALSE)
# Sem weekday e hour como fator
teste_final$hour <- as.factor(teste_final$hour) 
reg.log3 <- readRDS("reg.log3.rds")
teste_final1 <- teste_final[1:5000000,]
reg.log3.probs1 <- predict(reg.log3, newdata = teste_final1, type = "response")
teste_final2 <- teste_final[5000001:10000000,]
reg.log3.probs2 <- predict(reg.log3, newdata = teste_final2, type = "response")
teste_final3 <- teste_final[10000001:18790469,]
reg.log3.probs3 <- predict(reg.log3, newdata = teste_final3, type = "response")
df.reg.log3 <- data.frame(click_id = teste_final$click_id, is_attributed = c(reg.log3.probs1,reg.log3.probs2,reg.log3.probs3))
write.csv(df.reg.log3, file = "reg_log3.csv", row.names = FALSE)