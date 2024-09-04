
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(KoNLP)
library(tm)

# 데이터 불러오기
# 예시로 CSV 파일에서 데이터를 불러옵니다.
# 실제 데이터 파일 경로로 변경하세요.
patent_data <- read.csv("/Users/woosu/woodev/ip/example.csv", stringsAsFactors = FALSE)

# IPC 분류에서 서브클래스 추출 함수
extract_subclass <- function(ipc_code) {
  # IPC 코드를 "|"로 분할하고 각 부분에서 서브클래스 추출
  subclasses <- unlist(strsplit(ipc_code, "\\|"))
  subclasses <- gsub(" .*", "", subclasses) # 서브클래스 부분만 추출
  subclasses <- substr(subclasses, 1, 4)   # 첫 4자리만 남김
  return(paste(unique(subclasses), collapse = " "))
}

patent_data <- patent_data %>%
  mutate(IPC_subclass = sapply(Ipc, extract_subclass))


# IPC subclass가 하나인 행 제거
filtered_patent_data <- patent_data %>%
  filter(str_count(IPC_subclass, " ") >= 1)

# 특허 데이터의 초록과 청구항 합치기
patent_data <- patent_data %>%
  mutate(Text = paste(Sum, Claim, sep = " "))



# 특허-IPC 행렬 생성
# IPC subclass를 개별 열로 분할하고, 동일 특허 내 중복 제거
ipc_matrix <- filtered_patent_data %>%
  separate_rows(IPC_subclass, sep = " ") %>%
  distinct() %>%
  mutate(value = 1) %>%
  spread(key = IPC_subclass, value = value, fill = 0)

# 0번에서 5번까지의 열 제거
ipc_matrix <- ipc_matrix %>% select(-(1:6))

# IPC 간의 공동 출현 빈도 행렬 초기화
ipc_names <- colnames(ipc_matrix)
co_occurrence_matrix <- matrix(0, nrow = length(ipc_names), ncol = length(ipc_names))
rownames(co_occurrence_matrix) <- colnames(co_occurrence_matrix) <- ipc_names

# 공동 출현 빈도 계산
for (i in 1:nrow(ipc_matrix)) {
  current_row <- ipc_matrix[i, ]
  present_ipcs <- names(current_row)[current_row == 1]
  if (length(present_ipcs) > 1) {
    for (j in 1:(length(present_ipcs) - 1)) {
      for (k in (j + 1):length(present_ipcs)) {
        co_occurrence_matrix[present_ipcs[j], present_ipcs[k]] <- co_occurrence_matrix[present_ipcs[j], present_ipcs[k]] + 1
        co_occurrence_matrix[present_ipcs[k], present_ipcs[j]] <- co_occurrence_matrix[present_ipcs[k], present_ipcs[j]] + 1
      }
    }
  }
}
# 공동 출현 빈도 행렬을 데이터프레임으로 변환
ipc_co_occurrence_df <- as.data.frame(co_occurrence_matrix)

# 결과 확인
print(ipc_co_occurrence_df)
# IPC 추출 후 개수 확인
ipc_count_initial <- length(unique(unlist(strsplit(paste(patent_data$IPC_subclass, collapse = " "), " "))))
cat("R에서 초기 추출된 IPC 개수:", ipc_count_initial, "\n")

# 필터링 후 개수 확인
ipc_count_filtered <- length(unique(unlist(strsplit(paste(filtered_patent_data$IPC_subclass, collapse = " "), " "))))
cat("R에서 필터링 후 IPC 개수:", ipc_count_filtered, "\n")





