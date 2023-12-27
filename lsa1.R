# ������ ����-���������� �������
mat <- matrix(c(1, 1, 1, 0, 1, 1, 1, 0, 0, 0,
                1, 0, 1, 0, 0, 1, 0, 1, 1, 1,
                1, 1, 1, 1, 1, 0, 1, 0, 2, 0,
                0, 1, 1), ncol = 3, byrow = TRUE)

# ��������� ������������ �������� (SVD)
svd_result <- svd(mat)

# ������� U, S �� V
U <- svd_result$u
Sigma <- diag(svd_result$d)
V <- svd_result$v

# ���� ������� ��� (�������� k)
num_topics <- 2

# ��������� ������� ������� U, S �� V
U_reduced <- U[, 1:num_topics]
Sigma_reduced <- Sigma[1:num_topics, 1:num_topics]
V_reduced <- V[, 1:num_topics]

# ³��������� ������ ������� LSA
lsa_result <- U_reduced %*% Sigma_reduced %*% t(V_reduced)

# ��������� ����������
print("����-���������� �������:")
print(mat)
print("������� LSA:")
print(lsa_result)

# ������� ��� ����� (����� � �������)
doc1 <- lsa_result[1, ]
doc2 <- lsa_result[5, ]

# ���������� ��������� ��������
cosine_similarity <- sum(doc1 * doc2) / (sqrt(sum(doc1^2)) * sqrt(sum(doc2^2)))
print(paste("������� ��������:", cosine_similarity))

# ���������� �������� �������� ��� ����� ���� ������� � ������� LSA
cosine_similarity_matrix <- matrix(0, nrow = nrow(lsa_result), ncol = nrow(lsa_result))

for (i in 1:nrow(lsa_result)) {
  for (j in 1:nrow(lsa_result)) {
    vec1 <- lsa_result[i, ]
    vec2 <- lsa_result[j, ]
    
    cosine_similarity_matrix[i, j] <- sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
  }
}

# ��������� ������� ��������� ��������
print(cosine_similarity_matrix)

rownames(cosine_similarity_matrix) <- c("a", "arrived", "damaged", "delivery", "fire", "gold", "in", "of", "shipment", "silver", "truck")
colnames(cosine_similarity_matrix) <- c("a", "arrived", "damaged", "delivery", "fire", "gold", "in", "of", "shipment", "silver", "truck")

# ���������� �������� ��������

library(viridisLite)
ggplot(data = reshape2::melt(cosine_similarity_matrix),
       aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +
  scale_fill_viridis_c(option = "F", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "���������� �������� ��������",
       x = " ",
       y = " ")