library(readr)

data <- read_xlsx("/Users/woosu/data_v3.xlsx")
# UTF-8 인코딩으로 CSV 파일 읽기
data_split_csv <- read.csv("/Users/woosu/combined_split_ipc_words.csv")

head(data_split_csv, 5)


# IPC 코드들에서 중복 제거하고 벡터로 추출하는 R 코드
unique_ipc <- unique(unlist(strsplit(data$ipc, split = " \\| ")))

unique_ipc <- sort(unique_ipc)


# 결과 출력
length(unique_ipc)


# 빈 매트릭스 생성
binary_matrix <- matrix(0, nrow = length(data$ipc), ncol = length(unique_ipc))
colnames(binary_matrix) <- unique_ipc

# 각 행에 대해 IPC 코드를 확인하여 매트릭스에 반영
for (i in 1:length(data$ipc)) {
  ipc_codes <- unlist(strsplit(data$ipc[i], " \\| ")) # 각 행을 분리
  binary_matrix[i, ipc_codes] <- 1 # 해당하는 IPC 코드에 1을 할당
}


# IPC 간의 공동 등장 빈도를 나타내는 대칭 행렬
co_occurrence_matrix <- t(binary_matrix) %*% binary_matrix
# 대각선 값은 동일 IPC 간의 카운트이므로, 필요하면 0으로 설정
diag(co_occurrence_matrix) <- 0
# 결과를 데이터 프레임으로 변환
co_occurrence_df <- as.data.frame(co_occurrence_matrix)

print(co_occurrence_matrix)


print(head(data))

library(dplyr)
library(tidyr)

# IPC별로 각각 나누기
# 각 행에서 IPC 코드를 분리해서 long format으로 변환
data_long <- data %>%
  separate_rows(ipc, sep = " \\| ")  # IPC를 각각 행으로 분리

# IPC별로 terms를 그룹핑해서 합치기
result <- data_long %>%
  group_by(ipc) %>%
  summarise(terms_combined = paste(terms, collapse = " "))  # IPC별로 terms를 하나의 문자열로 합침

library(tidytext)
library(dplyr)
library(tm)

print(head(split_top_words))

# IPC별로 terms_combined 텍스트를 단어 단위로 분리
ipc_words <- result %>%
  unnest_tokens(word, terms_combined)  # terms_combined를 단어로 분리

# 4. 각 IPC와 단어별 빈도 계산
ipc_word_counts <- ipc_words %>%
  count(ipc, word, sort = TRUE)  # IPC별 단어 빈도 계산

# 5. TF-IDF 계산
ipc_tf_idf <- ipc_word_counts %>%
  bind_tf_idf(word, ipc, n)  # IPC별 TF-IDF 계산

# 6. 각 IPC별로 TF-IDF가 가장 높은 상위 100개 단어 추출
top_words_per_ipc <- ipc_tf_idf %>%
  group_by(ipc) %>%
  top_n(100, tf_idf) %>%
  arrange(ipc, desc(tf_idf))  # 각 IPC별로 상위 100개 단어 추출

# 결과 확인
print(top_words_per_ipc)

# 데이터 프레임을 5등분으로 나누기
num_splits <- 5
split_size <- ceiling(nrow(result) / num_splits)

# 빈 데이터프레임 생성
top_words_per_ipc <- data.frame()

for (i in 1:num_splits) {
  # 각 분할된 부분 추출
  split_data <- result[((i-1) * split_size + 1):min(i * split_size, nrow(result)), ]
  
  # 텍스트를 단어로 나누고 TF-IDF 계산 (부분 처리)
  split_ipc_words <- split_data %>%
    unnest_tokens(word, terms_combined) %>%
    anti_join(stop_words) %>%
    count(ipc, word, sort = TRUE) %>%
    bind_tf_idf(word, ipc, n)
  
  # 각 IPC별 상위 100개 단어 추출
  split_top_words <- split_ipc_words %>%
    group_by(ipc) %>%
    top_n(100, tf_idf) %>%
    arrange(ipc, desc(tf_idf))
  
  # 결과 병합
  top_words_per_ipc <- bind_rows(top_words_per_ipc, split_top_words)
  
  # 진행 상황 출력
  cat("Processed split:", i, "\n")
}

# 데이터 프레임을 5등분으로 나누기
num_splits <- 5
split_size <- ceiling(nrow(result) / num_splits)

# 빈 데이터프레임 생성
top_words_per_ipc <- data.frame()

for (i in 1:num_splits) {
  # 각 분할된 부분 추출
  split_data <- result[((i-1) * split_size + 1):min(i * split_size, nrow(result)), ]
  
  # 텍스트를 단어로 나누고 TF-IDF 계산 (부분 처리)
  split_ipc_words <- split_data %>%
    unnest_tokens(word, terms_combined) %>%
    anti_join(stop_words) %>%
    count(ipc, word, sort = TRUE) %>%
    bind_tf_idf(word, ipc, n)
  
  # 결과 병합 (전체 단어를 저장)
  top_words_per_ipc <- bind_rows(top_words_per_ipc, split_ipc_words)
  
  # split_ipc_words_n 데이터프레임 생성 (전체 단어 저장)
  assign(paste0("split_ipc_words_", i), split_ipc_words)
  
  # 진행 상황 출력
  cat("Processed split:", i, "\n")
}
# split_ipc_words_1부터 5까지 합치기
combined_split_ipc_words <- bind_rows(split_ipc_words_1, split_ipc_words_2, split_ipc_words_3, split_ipc_words_4, split_ipc_words_5)
# 결측치가 있는 행 제거
cleaned_combined_split_ipc_words <- na.omit(combined_split_ipc_words)
# 빈 행 제거 (모든 열이 NA 또는 공백인 행 제거)
cleaned_combined_split_ipc_words <- cleaned_combined_split_ipc_words[!apply(cleaned_combined_split_ipc_words == "", 1, all), ]

# CSV 파일로 저장 (CP949 인코딩)
csv_file_path <- "/Users/woosu/combined_split_ipc_words1.csv"
write.csv(combined_split_ipc_words, file = csv_file_path, row.names = FALSE, fileEncoding = "CP949")

cat("CSV 파일로 저장 완료:", csv_file_path, "\n")
# 결과 확인
print(top_words_per_ipc)

# split_ipc_words 데이터를 CSV로 저장하는 코드

# CSV 파일 경로 설정 (원하는 경로로 변경 가능)
csv_file_path <- "/Users/woosu/split_ipc_words.xlsx"

# 데이터프레임을 CSV 파일로 저장
write_xlsx(split_ipc_words, path = "split_ipc_words.xlsx")
write.csv(split_ipc_words, file = "split_ipc_words.csv", row.names = FALSE, fileEncoding = "CP949")
 
print(head(split_ipc_words, 50), n= 50)


# 저장된 파일 경로 출력
cat("CSV 파일이 저장되었습니다:", csv_file_path, "\n")

print(head(split_ipc_words))

# 결과 확인
print(top_words_per_ipc)

# IPC별로 상위 100개 단어를 정확하게 추출
top_100_words_per_ipc <- top_words_per_ipc %>%
  group_by(ipc) %>%
  arrange(desc(tf_idf)) %>%   # tf_idf 값을 기준으로 내림차순 정렬
  slice_head(n = 100)         # 각 IPC별로 상위 100개의 단어만 추출

head(top_100_words_per_ipc)






data_ipc <- data.frame(ipc = top_100_words_per_ipc$ipc, term = top_100_words_per_ipc$word)
write_xlsx(data_ipc, path = "data_ipc.xlsx")
# 필요한 패키지 로드
library(dplyr)
# IPC별로 단어를 쉼표로 구분하여 텍스트 파일에 저장하는 함수
save_ipc_words_to_txt <- function(data) {
  # 저장할 파일 경로 설정
  file_path <- "/Users/woosu/ipc_words.txt"
  
  # IPC별로 그룹화하여 각 IPC에 대한 단어를 쉼표로 연결
  ipc_words <- data %>%
    group_by(ipc) %>%
    summarize(words_combined = paste0('"', word, '"', collapse = ", "))
  
  # 파일에 쓰기
  writeLines(
    apply(ipc_words, 1, function(row) {
      paste0(row["ipc"], "\n", row["words_combined"], "\n")
    }), 
    con = file_path
  )
  
  # 생성된 파일 경로 반환
  return(file_path)
}
library(openxlsx)
library(writexl)
# 함수 호출 및 파일 저장
generated_file_path <- save_ipc_words_to_txt(top_100_words_per_ipc)

# 파일 저장 경로 출력
cat("파일이 저장된 경로:", generated_file_path, "\n")
