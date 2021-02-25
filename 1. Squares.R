
set.seed(5)
getwd()
install.packages('neuralnet')

setwd("C:/Program Files/R")
library('neuralnet')

mydata = read.csv('Squares.csv', sep=';', header = TRUE)
mydata
attach(mydata) 
names(mydata)



model = neuralnet(formula = Output ~ Input,
                  data = mydata,
                  hidden = c(9, 5),
                  threshold = 0.01)

print(model)

plot(model)

final_output = cbind(Input, Output,
                     as.data.frame(model$net.result))
colnames(final_output) = c('Input', 'Expected_output',
                           'Neuralnet_output')
print(final_output)

dif <- (final_output$Expected_output- final_output$Neuralnet_output)^2
dif
MSE <- sum(dif)/length(dif)


model2 = neuralnet(formula = Output ~ Input,
                  data = mydata,
                  hidden = c(3, 5, 7),
                  threshold = 0.01)

print(model2)

plot(model2)

final_output2 = cbind(Input, Output,
                     as.data.frame(model2$net.result))
colnames(final_output2) = c('Input', 'Expected_output',
                           'Neuralnet_output')
print(final_output2)

dif2 <- (final_output2$Expected_output- final_output2$Neuralnet_output)^2
dif2
MSE2 <- sum(dif2)/length(dif2)


model3 = neuralnet(formula = Output ~ Input,
                   data = mydata,
                   hidden = c(2, 6, 3),
                   threshold = 0.01)

print(model3)

plot(model3)

final_output3 = cbind(Input, Output,
                      as.data.frame(model3$net.result))
colnames(final_output3) = c('Input', 'Expected_output',
                            'Neuralnet_output')
print(final_output3)

dif3 <- (final_output3$Expected_output- final_output3$Neuralnet_output)^2
dif3
MSE3 <- sum(dif3)/length(dif3)


model4 = neuralnet(formula = Output ~ Input,
                   data = mydata,
                   hidden = c(8),
                   threshold = 0.01)

print(model4)

plot(model4)

final_output4 = cbind(Input, Output,
                      as.data.frame(model4$net.result))
colnames(final_output4) = c('Input', 'Expected_output',
                            'Neuralnet_output')
print(final_output4)

dif4 <- (final_output4$Expected_output- final_output4$Neuralnet_output)^2
dif4
MSE4 <- sum(dif4)/length(dif4)


model5 = neuralnet(formula = Output ~ Input,
                   data = mydata,
                   hidden = c(2, 7),
                   threshold = 0.01)

print(model5)

plot(model5)

final_output5 = cbind(Input, Output,
                      as.data.frame(model5$net.result))
colnames(final_output5) = c('Input', 'Expected_output',
                            'Neuralnet_output')
print(final_output5)

dif5 <- (final_output5$Expected_output- final_output5$Neuralnet_output)^2
dif5
MSE5 <- sum(dif5)/length(dif5)

# Лучшая модель из 2-4, является модель 2, т.к 
# ошибка MSE для нее является самой маленькой