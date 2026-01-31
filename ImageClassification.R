options("install.lock"=FALSE)

library(keras)
library(tensorflow)

reticulate::install_python()

keras::install_keras() 


fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train

dim(train_images)
c(test_images, test_labels) %<-% fashion_mnist$test
dim(test_images)

class_names<- list(c("T-Shirt", "Pants", "Sweater", "Dress", "Coat", "Sandals", "Shirt", "Sneakers", "Bag", "Boots"))

class_names


corrplot::corrplot(train_images[1,,], is.corr=F, method="color", tl.pos="n")

image(train_images[1,,],useRaster=T,axes=F)
image(t(apply(train_images[1,,], 2, rev)),useRaster=T,axes=F)


image_1 <- as.data.frame(train_images[1, , ])
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)
ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")


image_2 <- as.data.frame(train_images[2, , ])
colnames(image_2) <- seq_len(ncol(image_2))
image_2$y <- seq_len(nrow(image_2))
image_2 <- gather(image_2, "x", "value", -y)
image_2$x <- as.integer(image_2$x)
ggplot(image_2, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")




image_3 <- as.data.frame(test_images[1, , ])
colnames(image_3) <- seq_len(ncol(image_3))
image_3$y <- seq_len(nrow(image_3))
image_3 <- gather(image_3, "x", "value", -y)
image_3$x <- as.integer(image_3$x)
ggplot(image_3, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

train_images2 <- train_images / 255
test_images2 <- test_images / 255

par(mfcol=c(3,3))
for (i in 1:9) {
  image(t(apply(train_images2[i, , ], 2, rev)), xaxt = 'n', yaxt = 'n',
        main = paste(class_names[train_labels[i] + 1]))
}

model <- keras_model_sequential()
model %>% layer_flatten(input_shape = c(28, 28)) %>%  layer_dense(units = 128, activation = 'relu') %>%  layer_dense(units = 10, activation = 'softmax')


#hyperparameters
model %>% compile(optimizer = 'adam', loss = 'sparse_categorical_crossentropy', metrics = c('accuracy'))

#set parameters
model %>% fit(train_images, train_labels, epochs = 5, verbose = 2)

score <- model %>% evaluate(test_images, test_labels, verbose = 0)
print(score["loss"])
print(score["accuracy"])

score2 <- model %>% evaluate(train_images, train_labels, verbose = 0 )
print(score2["loss"])
print(score2["accuracy"])



predictions <- model %>% predict(test_images)
print(predictions[1, ])
print(which.max(predictions[1, ]))
print(test_labels[1])


predictions_1 <- model %>% predict(test_images[1, , , drop = FALSE])
which.max(predictions_1 )


predictions_test <- model %>% predict(test_images)
predictions_test_max = max.col(predictions_test)
test_labels1 = test_labels+1
test_pred_lab = cbind(test_labels1,predictions_test_max)
colnames(test_pred_lab) = c("Observed","Predicted")
table(as.data.frame(test_pred_lab))
corrplot::corrplot(table(as.data.frame(test_pred_lab)),
                   is.corr=F,method="color")

