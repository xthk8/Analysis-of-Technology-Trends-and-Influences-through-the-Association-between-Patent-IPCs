# readr 패키지 로드
library(readr)
library(writexl)
library(readxl)
# 파일 경로를 사용하여 엑셀 파일 읽기
data <- read_csv("/Users/woosu/woodev/ipc/top_100_words_all_ipc_finfin.csv")

# 결측치 제거 (NA 값이 포함된 행을 제거)
data <- na.omit(data)


head(data, 5)


# IPC 코드들에서 중복 제거하고 벡터로 추출하는 R 코드
unique_ipc <- unique(unlist(strsplit(data$ipc, split = " \\| ")))

unique_ipc <- sort(unique_ipc)


# 데이터 불러오기 (이미 이 작업은 완료된 상태)
datax <- read_xlsx("/Users/woosu/data_v3.xlsx")

# IPC 코드 분리 및 리스트로 변환
ipc_combinations <- strsplit(datax$ipc, split = " \\| ")
print(ipc_combinations)

# IPC 쌍들의 공통 출현 빈도 계산
ipc_pairs <- lapply(ipc_combinations, function(codes) {
  combn(codes, 2, simplify = FALSE)
})

# 모든 쌍들을 하나의 벡터로 병합
ipc_pairs <- unlist(ipc_pairs, recursive = FALSE)

# 리스트를 데이터프레임으로 변환하고, 각 쌍의 빈도 계산
ipc_df <- data.frame(t(sapply(ipc_pairs, function(x) c(sort(x))))) %>%
  rename(IPC1 = X1, IPC2 = X2)
# 613개 중 2개를 고르는 조합의 수 계산
combinations <- choose(634, 2)

# 결과 출력
print(combinations)

#### <표 9> IPC조합 관점에서의 공동 출현 빈도 상위 20개
ipc_freq <- ipc_df %>%  
  group_by(IPC1, IPC2) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# 상위 20개만 선택
top_100_ipc_freq <- head(ipc_freq, 100)

print(head(top_100_ipc_freq))


# 데이터 xlsx 파일로 저장
write_xlsx(top_100_ipc_freq, "top_100_ipc_freq.xlsx")



# 1. IPC 서브클래스 추출 (각 IPC 코드의 앞 4자리만 추출)
ipc_subclasses <- lapply(ipc_combinations, function(codes) {
  substr(codes, 1, 4)  # 각 IPC 코드에서 서브클래스 추출 (앞 4글자)
})

# 2. 각 특허에 등장하는 서브클래스들의 빈도 계산
ipc_subclass_counts <- table(unlist(ipc_subclasses))

# 3. 데이터프레임으로 변환하여 빈도순으로 정렬
ipc_subclass_df <- as.data.frame(ipc_subclass_counts)
names(ipc_subclass_df) <- c("IPC", "Count")

# 4. 상위 10개의 서브클래스 추출
ipc_subclass_df <- ipc_subclass_df %>%
  arrange(desc(Count)) 

# 결과 출력
print(top_100_ipc_subclasses)

# 파일 경로를 사용하여 엑셀 파일 읽기
data <- read_csv("/Users/woosu/Desktop/ip_final/ipc_top100_words.csv")

head(data)

# library(dplyr)
# 
# # 각 IPC별 빈도 계산
# ipc_counts <- data %>%
#   count(ipc) %>%
#   arrange(desc(n))  # 빈도(n) 기준으로 내림차순 정렬
# 
# # 결과 출력
# print(ipc_counts, n = 634)
# data_test <- read_csv('/Users/woosu/woodev/ipc_descent/combined_split_ipc_words.csv')
# 
# head(data_test)
# 
# # 각 IPC별 빈도 계산
# ipc_countss <- data_test %>%
#   count(ipc)
# # 결과 출력
# print(ipc_counts, n = 634)
# 

#####################################################################


library(dplyr)

# 1. CSV 파일 읽기
data <- read_csv("/Users/woosu/woodev/ipc/top_100_words_all_ipc_finfin.csv")

# 2. IPC별 단어 리스트 만들기
ipc_words <- data %>%
  group_by(ipc) %>%
  summarise(word_list = list(word))


# 3. 두 개의 IPC 조합별로 중복된 단어 개수 계산
ipc_combinations <- combn(unique(data$ipc), 2, simplify = FALSE)

# 4. IPC 조합별로 중복 단어 개수 세기
common_word_counts <- lapply(ipc_combinations, function(ipcs) {
  ipc1 <- ipcs[1]
  ipc2 <- ipcs[2]
  
  # ipc1과 ipc2의 단어 리스트
  words1 <- unlist(ipc_words$word_list[ipc_words$ipc == ipc1])
  words2 <- unlist(ipc_words$word_list[ipc_words$ipc == ipc2])
  
  # 공통 단어 개수 계산
  common_words <- intersect(words1, words2)
  
  # 결과 저장 (IPC 조합과 공통 단어 개수)
  data.frame(IPC1 = ipc1, IPC2 = ipc2, common_word_count = length(common_words))
})

# 5. 결과를 하나의 데이터프레임으로 병합
common_word_counts_df <- bind_rows(common_word_counts)

# 6. 공통 단어 개수가 많은 순으로 정렬
common_word_counts_df <- common_word_counts_df %>% 
  arrange(desc(common_word_count))

# 결과 출력
print(common_word_counts_df)

# 상위 20개만 선택
top_1_ipc_common_word <- head(common_word_counts_df, 1)
print(top_1_ipc_common_word)

# 데이터 xlsx 파일로 저장
write_xlsx(top_100_ipc_common_word, "top_100_ipc_common_word.xlsx")
library(dplyr)


# 두 데이터프레임을 IPC1_ordered와 IPC2_ordered를 기준으로 병합
merged_df <- merge(top_100_ipc_common_word, top_100_ipc_freq, 
                   by = c("IPC1_ordered", "IPC2_ordered"))

# 최종 데이터프레임에 필요한 열만 선택
final_df <- merged_df %>%
  select(IPC1_ordered, IPC2_ordered, common_word_count, count)

# 결과 출력
print(length(final_df$IPC1_ordered))

write_xlsx(final_df, "ipc_final_df.xlsx")

#############################################################3


head(common_word_counts_df)

head(ipc_freq)

head(ipc_subclass_df)

head(tech_influence)

# 1. 전체 IPC 등장 횟수 계산 (전체 등장 횟수 합계)
total_ipc_count <- sum(ipc_subclass_df$Count)

# 2. 지지도 계산: 각 IPC 등장 빈도 / 전체 등장 횟수
ipc_subclass_df <- ipc_subclass_df %>%
  mutate(support = Count / total_ipc_count)

# 3. 최소 지지도 0.05% (0.0005) 설정하고 그 이상인 IPC만 필터링
min_support_threshold <- 0.0005
filtered_ipc_subclass_df <- ipc_subclass_df %>%
  filter(support >= min_support_threshold)

# 제외된 IPC 개수 계산
excluded_ipc_count <- nrow(ipc_subclass_df) - nrow(filtered_ipc_subclass_df)
print(paste("제외된 IPC 개수: ", excluded_ipc_count))

# 4. 필터링된 IPC에 대해서만 신뢰도 계산 수행
ipc_with_freq <- ipc_freq %>%
  filter(IPC1 %in% filtered_ipc_subclass_df$IPC) %>% # 필터링된 IPC만 포함
  left_join(filtered_ipc_subclass_df, by = c("IPC1" = "IPC"))

# 신뢰도 계산: 두 IPC가 함께 등장한 빈도 / 첫 번째 IPC의 등장 횟수
ipc_with_freq <- ipc_with_freq %>%
  mutate(confidence = count / Count)

# 5. 최소 신뢰도 0.05% (0.0005) 이상인 조합만 필터링
min_confidence_threshold <- 0.0005
filtered_ipc_with_freq <- ipc_with_freq %>%
  filter(confidence >= min_confidence_threshold)

# 제외된 IPC 조합 개수 계산
excluded_ipc_combinations <- nrow(ipc_with_freq) - nrow(filtered_ipc_with_freq)
print(paste("제외된 IPC 조합 개수: ", excluded_ipc_combinations))

# 6. 기술의 영향력 계산: IPC2별로 신뢰도를 합산
tech_influence <- filtered_ipc_with_freq %>%
  group_by(IPC2) %>%
  summarise(tech_influence = sum(confidence, na.rm = TRUE)) %>%
  arrange(desc(tech_influence))
# IPC2별로 그룹화하고 신뢰도의 합산을 계산
tech_influence <- filtered_ipc_with_freq %>%
  group_by(IPC2) %>%
  summarise(
    tech_influence = sum(confidence, na.rm = TRUE) * n(),  # 합산된 신뢰도에 그룹화된 행의 개수를 곱함
    count = n()  # 그룹화된 행의 개수 확인용
  ) %>%
  arrange(desc(tech_influence))  # 기술의 영향력이 큰 순서대로 정렬
# 결과 확인
print(tech_influence)
# 6. 기술의 영향력 계산: IPC2별로 신뢰도를 합산
tech_influence_1 <- filtered_ipc_with_freq %>%
  group_by(IPC2) %>%
  summarise(
    tech_influence = sum(confidence, na.rm = TRUE) * n(),  # 합산된 신뢰도에 그룹화된 행의 개수를 곱함
    count = n(),  # 그룹화된 행의 개수 확인용
    sum_confidence = sum(confidence, na.rm = TRUE)  # 신뢰도의 Summation 값 추가
  ) %>%
  arrange(desc(tech_influence))  # 기술의 영향력이 큰 순서대로 정렬

# 결과 확인
print(tech_influence)
# 결과를 엑셀 파일로 저장
write.csv(tech_influence_1, "tech_influence1.csv", row.names = FALSE)

library(dplyr)
library(writexl)

# 이미 불러온 데이터들
# common_word_counts_df: IPC 조합별 공통 단어 개수
# ipc_freq: IPC 조합별 등장 빈도
# ipc_subclass_df: 각 IPC의 등장 빈도 및 지지도
# tech_influence: IPC2별 기술 영향력 및 관련 신뢰도 count

# 1. common_word_counts_df와 ipc_freq를 IPC1과 IPC2를 기준으로 병합
merged_df <- common_word_counts_df %>%
  left_join(ipc_freq, by = c("IPC1", "IPC2"))

# 2. ipc_subclass_df를 IPC1 기준으로 병합하여 support 값 및 Count 값을 추가
merged_df <- merged_df %>%
  left_join(ipc_subclass_df %>% select(IPC, support, Count), by = c("IPC1" = "IPC"))

# 3. tech_influence 데이터프레임을 IPC2 기준으로 병합하여 기술 영향력과 신뢰도 count를 추가
merged_df <- merged_df %>%
  left_join(tech_influence, by = "IPC2")

# 4. 최종 결과를 엑셀 파일로 저장
write_xlsx(merged_df, "ipc_analysis.xlsx")

print("데이터가 성공적으로 병합되고 저장되었습니다.")
library(dplyr)
library(writexl)

# 이미 불러온 데이터들
# common_word_counts_df: IPC 조합별 공통 단어 개수
# ipc_freq: IPC 조합별 등장 빈도
# ipc_subclass_df: 각 IPC의 등장 빈도 및 지지도
# tech_influence: IPC2별 기술 영향력 및 관련 신뢰도 count

# 1. common_word_counts_df와 ipc_freq를 IPC1과 IPC2를 기준으로 병합
merged_df <- common_word_counts_df %>%
  left_join(ipc_freq, by = c("IPC1", "IPC2"))

# 2. ipc_subclass_df에서 IPC1 기준으로 Count 값을 가져와 merged_df에 추가
# 이 때, IPC1과 IPC가 일치하는지 확인하고, Count 값을 추가
merged_df <- merged_df %>%
  left_join(ipc_subclass_df %>% select(IPC, Count), by = c("IPC1" = "IPC"))

# 3. tech_influence 데이터프레임을 IPC2 기준으로 병합하여 기술 영향력과 신뢰도 count를 추가
merged_df <- merged_df %>%
  left_join(tech_influence, by = "IPC2")

# 4. 최종 결과를 엑셀 파일로 저장
write_xlsx(merged_df, "ipc_analysis_with_count.xlsx")

# 결과 확인
print(head(merged_df))

library(dplyr)
library(writexl)

# 이미 불러온 데이터들
# common_word_counts_df: IPC 조합별 공통 단어 개수
# ipc_freq: IPC 조합별 등장 빈도
# ipc_subclass_df: 각 IPC의 등장 빈도 및 지지도
# tech_influence: IPC2별 기술 영향력 및 관련 신뢰도 count

# 1. 먼저 common_word_counts_df와 ipc_freq를 IPC1과 IPC2를 기준으로 병합
merged_df <- common_word_counts_df %>%
  left_join(ipc_freq, by = c("IPC1", "IPC2"))

# 2. IPC1과 IPC가 일치하는 경우에만 Count 값을 병합
merged_df$Count_IPC1 <- sapply(merged_df$IPC1, function(x) {
  match_count <- ipc_subclass_df$Count[ipc_subclass_df$IPC == x]
  if(length(match_count) > 0) {
    return(match_count)
  } else {
    return(NA)
  }
})

# 3. tech_influence 데이터프레임을 IPC2 기준으로 병합하여 기술 영향력과 신뢰도 count를 추가
merged_df <- merged_df %>%
  left_join(tech_influence, by = "IPC2")

# 4. 최종 결과를 엑셀 파일로 저장
write_xlsx(merged_df, "ipc_analysis_with_direct_count.xlsx")

# 결과 확인
print(head(merged_df))


library(dplyr)
library(writexl)

# 이미 불러온 데이터들
# common_word_counts_df: IPC 조합별 공통 단어 개수
# ipc_freq: IPC 조합별 등장 빈도
# ipc_subclass_df: 각 IPC의 등장 빈도 및 지지도
# tech_influence: IPC2별 기술 영향력 및 관련 신뢰도 count

library(dplyr)
library(writexl)

# 이미 불러온 데이터들
# common_word_counts_df: IPC 조합별 공통 단어 개수
# ipc_freq: IPC 조합별 등장 빈도
# ipc_subclass_df: 각 IPC의 등장 빈도 및 지지도
# tech_influence: IPC2별 기술 영향력 및 관련 신뢰도 count

# 1. IPC1, IPC2 순서 상관없이 같은 조합을 동일하게 취급하기 위해 IPC1, IPC2를 정렬하여 새로운 열 생성
ipc_freq <- ipc_freq %>%
  mutate(IPC_comb = paste(pmin(IPC1, IPC2), pmax(IPC1, IPC2), sep = "_"))

common_word_counts_df <- common_word_counts_df %>%
  mutate(IPC_comb = paste(pmin(IPC1, IPC2), pmax(IPC1, IPC2), sep = "_"))

# 2. common_word_counts_df와 ipc_freq를 IPC_comb 기준으로 병합
merged_df <- common_word_counts_df %>%
  left_join(ipc_freq %>% select(IPC_comb, count), by = "IPC_comb")

# 3. ipc_subclass_df에서 IPC1 기준으로 Count 값을 가져와 merged_df에 추가
merged_df <- merged_df %>%
  left_join(ipc_subclass_df %>% select(IPC, Count), by = c("IPC1.x" = "IPC"))

# 4. tech_influence 데이터프레임을 IPC2 기준으로 병합하여 기술 영향력과 신뢰도 count를 추가
merged_df <- merged_df %>%
  left_join(tech_influence, by = "IPC2")

# 5. 최종 결과를 엑셀 파일로 저장
write_xlsx(merged_df, "ipc_analysis_with_comb_count.xlsx")

# 결과 확인
print(head(merged_df))
