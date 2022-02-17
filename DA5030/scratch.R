sapply(df, function(x) {
    replace(x, which(x == 0), median(x))
})

colSums <- function(x) {
    sapply(x, function(x) {
        sum(x)
    })
}

a <- lapply(goal_z, function(x){
     ((x - sample_train[1:10, ]))
})
a
class(a)

sweep( as.matrix(sample_train), 2, as.matrix(goal_z[1,])) %>% as_tibble()

FindMode <- function(x) {
    ## Taken from https://www.delftstack.com/howto/r/mode-in-r/.
    u <- unique(x)
    u[which.max(tabulate(match(x, u)))]
}

my_knn <- function( train, test, cl, k){
    modes_output <- vector()
    for (i in 1:nrow(test)) {
        a <- sweep( as.matrix(train), 2, as.matrix(test[i, ])) %>% as_tibble()
        top_index <- sort(rowSums(a^2), index.return=T)$ix[1:k]
        mode <- sample_train_label[top_index] %>% FindMode()    
        modes_output <- append(modes_output, mode)
    }
    return (modes_output)
}

top_index <- sort(rowSums(a^2), index.return=T)$ix[1:5]
top_index
my_knn(sample_train, sample_test, sample_train_label, 5)

library(class)
knn(sample_train, goal_z, sample_train_label, 5)

a <- sweep( as.matrix(sample_train), 2, as.matrix(sample_test[1,])) %>% as_tibble()
a
top_index <- sort(rowSums(a^2), index.return=T)$ix[1:5]
mode <- sample_train_label[top_index] %>% FindMode()    
b <- vector()
append(b, mode)
b

knn.reg <- function(new_data, target_data, train_data, k ){
    if (k < 4) {stop("K must greater than 3.")}
    # Data normalization using min-max method.
    train_min <- min(train_data)
    train_max <- max(train_data)
    
    train_data_norm <- ((train_data - train_min) / (train_max - train_min))
    train_data_norm <- ((train_data - train_min) / (train_max - train_min))
    new_data_norm <- ((new_data - train_min) / (train_max - train_min))
    
    prices <- vector()
    for (i in 1:nrow(new_data_norm)) {
        a <- sweep( as.matrix(train_data_norm), 2, as.matrix(new_data_norm)) %>% as_tibble()
        top_index <- sort(rowSums(a^2), index.return=T)$ix[1:k]
        k <- 5
        weight_factor <- c(3,2,rep(1, k-2))
        price <- sum(target_data[top_index] * weight_factor / sum(weight_factor))
        prices <- append(prices, price)
    }
    return (prices)
}

new_data <- tibble( crim = 0.15560, zn = 12.5,
                    indus = 7.87, chas = 0,
                    nox = 0.524, Rm = 6.173, age = 96.1,
                    dis = 5.9505, rad = 5, tax = 311, 
                    pratio = 15.2, black = 396.9, lstat = 19.5)

knn.reg( new_data, target_data, train_data, 5)


prices <- vector()
a <- sweep( as.matrix(train_data), 2, as.matrix(new_data_norm)) %>% as_tibble()
top_index <- sort(rowSums(a^2), index.return=T)$ix[1:k]
k <- 5
weight_factor <- c(3,2,rep(1, k-2))
b <- sum(target_data_norm[top_index] * weight_factor / sum(weight_factor))
price <- b * (max(target_data) - min(target_data)) + min(target_data)

train_data_norm
new_data <- tibble( crim = 0.15560, zn = 12.5,
                    indus = 7.87, chas = 0,
                    nox = 0.524, Rm = 6.173, age = 96.1,
                    dis = 5.9505, rad = 5, tax = 311, 
                    pratio = 15.2, black = 396.9, lstat = 19.5)
new_data_norm <- (new_data - sapply(train_data, min)) / (sapply(train_data, max) - sapply(train_data, min))

for (i in 1:10){
    a <- sweep(as.matrix(train_data_norm[1:10, ]), 2, as.matrix(train_data_norm[i, ]))
    print(a)
    }

