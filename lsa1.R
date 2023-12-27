# Задаємо терм-документну матрицю
mat <- matrix(c(1, 1, 1, 0, 1, 1, 1, 0, 0, 0,
                1, 0, 1, 0, 0, 1, 0, 1, 1, 1,
                1, 1, 1, 1, 1, 0, 1, 0, 2, 0,
                0, 1, 1), ncol = 3, byrow = TRUE)

# Виконання сингулярного розкладу (SVD)
svd_result <- svd(mat)

# Матриці U, S та V
U <- svd_result$u
Sigma <- diag(svd_result$d)
V <- svd_result$v

# Вибір кількості тем (значення k)
num_topics <- 2

# Отримання усічених матриць U, S та V
U_reduced <- U[, 1:num_topics]
Sigma_reduced <- Sigma[1:num_topics, 1:num_topics]
V_reduced <- V[, 1:num_topics]

# Відновлення усіченої матриці LSA
lsa_result <- U_reduced %*% Sigma_reduced %*% t(V_reduced)

# Виведення результатів
print("Терм-документна матриця:")
print(mat)
print("Матриця LSA:")
print(lsa_result)

# Обираємо два слова (рядки у матриці)
doc1 <- lsa_result[1, ]
doc2 <- lsa_result[5, ]

# Розрахунок косинусної подібності
cosine_similarity <- sum(doc1 * doc2) / (sqrt(sum(doc1^2)) * sqrt(sum(doc2^2)))
print(paste("Êîñèíóñ ïîä³áíîñò³:", cosine_similarity))

# Розрахунок косинусів подібності для кожної пари векторів у матриці LSA
cosine_similarity_matrix <- matrix(0, nrow = nrow(lsa_result), ncol = nrow(lsa_result))

for (i in 1:nrow(lsa_result)) {
  for (j in 1:nrow(lsa_result)) {
    vec1 <- lsa_result[i, ]
    vec2 <- lsa_result[j, ]
    
    cosine_similarity_matrix[i, j] <- sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
  }
}

# Виведення матриці косинусної подібності
print(cosine_similarity_matrix)

rownames(cosine_similarity_matrix) <- c("a", "arrived", "damaged", "delivery", "fire", "gold", "in", "of", "shipment", "silver", "truck")
colnames(cosine_similarity_matrix) <- c("a", "arrived", "damaged", "delivery", "fire", "gold", "in", "of", "shipment", "silver", "truck")

# Теплокарта косинусів подібності

library(viridisLite)
ggplot(data = reshape2::melt(cosine_similarity_matrix),
       aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +
  scale_fill_viridis_c(option = "F", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Теплокарта косинусів подібності",
       x = " ",
       y = " ")
