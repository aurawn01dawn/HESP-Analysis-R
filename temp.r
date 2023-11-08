# 这里只是存储了用到的相关代码，要运行请分节运行，或使用该路径下的".rmd"文件

# 导入相关工具包（库）
library(tidyverse)
library(rattle)
library(caret)
# read
dset.0 <- read_csv("./data_csv/student_prediction.csv")
# tidy
dset_im.0 <- dset.0
names(dset_im.0) <- tolower(names(dset.0))
dset_im.0 <- dplyr::rename(dset_im.0, parents_status = kids)
# delete first column
vars_im <- c("gender", "scholarship", "work", "activity", "partner", 
             "parents_status", "study_hrs", "attend_dept", "grade")
# select
dset_im.1 <- dset_im.0 |>
  select(one_of(vars_im))
# recode
dset_im.2 <- dset_im.1
dset_im.2$gender <- recode(dset_im.1$gender, '1' = 'Female', '2' ='Male')
dset_im.2$scholarship <- recode(dset_im.1$scholarship, '1' = '0%', '2' = '25%', 
                                '3' = '50%', '4'= '75%', '5'= '100%')
dset_im.2$work <- recode(dset_im.1$work, '1' = 'Yes', '2' = 'No')
dset_im.2$activity <- recode(dset_im.1$activity, '1' = 'Yes', '2' = 'No')
dset_im.2$partner <- recode(dset_im.1$partner, '1' = 'Yes', '2' = 'No')
dset_im.2$parents_status <- recode(dset_im.1$parents_status, '1' = 'Married', 
                          '2' = 'Divorced', '3' = 'Died - one of them or both')
dset_im.2$study_hrs <- recode(dset_im.1$study_hrs, '1' = 'None', 
                    '2' = '<5 hours', '3' = '6-10 hours', '4' = '11-20 hours', 
                    '5' = 'More than 20 hours')
dset_im.2$attend_dept <- recode(dset_im.1$attend_dept, '1' = 'Yes', '2' = 'No')
# visualization 8 images total
dset_im.2 |>
ggplot(aes(grade)) + 
  geom_density(aes(fill = gender), alpha = 0.5) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade, group = scholarship)) +
  geom_bar(aes(fill = scholarship)) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade)) +
  geom_density(aes(fill = work), alpha = 0.5) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade)) +
  geom_density(aes(fill = activity), alpha = 0.5) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade)) +
  geom_density(aes(fill = partner), alpha = 0.5) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade, group = parents_status)) +
  geom_bar(aes(fill = parents_status)) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade, group = study_hrs)) + 
  geom_bar(aes(fill = study_hrs)) +
  coord_flip()
dset_im.2 |>
ggplot(aes(grade)) +
  geom_density(aes(fill = attend_dept), alpha = 0.5) +
  coord_flip()
# data model 
# 分层抽样
set.seed(777)  # 设置随机种子
# 按照目标进行8：2的分层抽样
train_list <- createDataPartition(dset_im.2$grade, p = 0.8, list = FALSE)
train <- dset_im.2[train_list,]
test <- dset_im.2[-train_list,]
# 将因变量转为因子类型
train$grade <- as.factor(train$grade)
test$grade <- as.factor(test$grade)
#构建模型的控制对象（采用5折交叉验证）
trControl<-trainControl(method = 'cv',number = 5)
set.seed(777)
# 随机森林模型构建
model_rf<-train(grade~.,data = train, method='rf',trControl=trControl)
# 对测试集进行预测
pred_rf<-predict(model_rf,test)
# 单变量直接用plot函数画出即可
plot(varImp(model_rf))