# Lấy đường dẫn tuyệt đối của file đang mở
current_path <- rstudioapi::getActiveDocumentContext()$path  
# Lấy thư mục chứa file
current_dir <- dirname(current_path)
# Set working directory về đúng chỗ đó
setwd(current_dir)
dir()

library(knitr)

spin("Bai1_GPU.R", knit = FALSE)  # tạo file .Rmd
knitr::knit2html("Bai1_GPU.R")  # hoặc knit2pdf

#a) ĐỌC VÀ XỬ LÍ DATA

data_raw <- read.csv("datasets/All_GPUs.csv", sep = ",", na.strings = c("", " ", "-", " -  ", "NA", "N/A","\nUnknown Release Date "))  

dim(data_raw)
head(data_raw)

# Lưu data_raw thành file CSV mới trong thư mục datasets
write.csv(
  data_raw,
  file = "datasets/All_GPUs_withNA.csv",
  row.names = FALSE  # bỏ cột số thứ tự tự động
)

# Tạo danh sách cột cần xử lý cùng với đơn vị cần xóa
cols_units <- list(
  "Memory_Bandwidth" = "GB/sec",
  "Core_Speed"       = " MHz",
  "Boost_Clock"      = " MHz",
  "Memory_Speed"     = " MHz",
  "Pixel_Rate"       = " GPixel/s",
  "Texture_Rate"     = " GTexel/s",
  "Direct_X"         = "DX",
  "L2_Cache"         = "KB",
  "Max_Power"        = " Watts",
  "Memory"           = "MB",
  "Memory_Bus"       = " Bit",
  "Process"          = "nm",
  "Release_Price"    = "\\$"
)
# Lặp qua từng cột để xoá đơn vị và chuyển sang numeric
for (col in names(cols_units)) {
  unit <- cols_units[[col]]
  data_raw[[col]] <- gsub(unit, "", data_raw[[col]])
  data_raw[[col]] <- as.numeric(data_raw[[col]])
}


# Function to parse and compute ROP values: convert "a (xb)" to a * b, or keep plain "a"
parse_rop <- function(rop_str) {
  # Remove spaces and handle NA
  if (is.na(rop_str) || rop_str == "") return(NA)
  
  # Use regex to match "a (xb)" pattern
  pattern <- "^(\\d+)\\s*\\(x(\\d+)\\)$"
  if (grepl(pattern, rop_str)) {
    # Extract a and b
    matches <- regmatches(rop_str, regexec(pattern, rop_str))
    a <- as.numeric(matches[[1]][2])
    b <- as.numeric(matches[[1]][3])
    return(a * b)
  } else {
    # If no "(x...)", assume plain number
    return(as.numeric(rop_str))
  }
}

data_raw$ROPs <- sapply(data_raw$ROPs, parse_rop)

#b) THỐNG KÊ MÔ TẢ

library(dplyr)
library(ggplot2)
library(lubridate)

# --- CHUẨN BỊ DỮ LIỆU: trích năm, tháng từ Release_Date ---
data_raw <- data_raw %>%
  mutate(
    # Chuyển Release_Date sang Date
    Release_Date = parse_date_time(Release_Date, orders = c("dmy","ymd"), quiet = TRUE),
    # Năm, tháng và quý
    Release_Year = year(Release_Date),
    Release_Month = month(Release_Date, label = TRUE, abbr = TRUE),
    Release_Quarter = paste0(Release_Year, " Q", quarter(Release_Date))
  )

# --- 1. Tỉ lệ manufacturer trong data---
manufacturer_counts <- data_raw %>%
  count(Manufacturer, name = "Count") %>%
  filter(!is.na(Manufacturer))

ggplot(manufacturer_counts, aes(x = "", y = Count, fill = Manufacturer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Market Share by Manufacturer") +
  theme_void() +  # bỏ trục
  theme(legend.position = "right")

# --- 2. Count of models released theo năm ---
release_year_count <- data_raw %>%
  filter(!is.na(Release_Year)) %>%
  count(Release_Year, name = "Count")

ggplot(release_year_count, aes(x = Release_Year, y = Count)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "orange") +
  labs(title = "Number of GPU Models Released per Year", x = "Year", y = "Count")

# --- 3. Count of models released theo tháng (tổng qua tất cả các năm) ---
release_month_count <- data_raw %>%
  filter(!is.na(Release_Month)) %>%
  count(Release_Month, name = "Count") %>%
  mutate(Month_Num = match(as.character(Release_Month), month.abb)) %>%
  arrange(Month_Num)

ggplot(release_month_count, aes(x = Month_Num, y = Count)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "orange", size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Number of GPU Models Released per Month",
       x = "Month",
       y = "Count")
# --- 4. Manufacturers counts by release year ---
manufacturer_year <- data_raw %>%
  filter(!is.na(Manufacturer) & !is.na(Release_Year)) %>%
  count(Release_Year, Manufacturer, name = "Count")

ggplot(manufacturer_year, aes(x = Release_Year, y = Count, color = Manufacturer)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Manufacturer Counts by Release Year", x = "Year", y = "Count")

# --- 5. Price distribution ---
# a) overall price distribution
price_year <- data_raw %>%
  filter(!is.na(Release_Price) & !is.na(Release_Year)) %>%
  group_by(Release_Year) %>%
  summarise(Avg_Price = mean(Release_Price))

ggplot(price_year, aes(x = Release_Year, y = Avg_Price)) +
  geom_line(color = "orange", size = 1.2) +
  geom_point(color = "orange") +
  labs(title = "Average GPU Price per Year", x = "Year", y = "Avg Price ($)")

# b) price by manufacturer
ggplot(data_raw %>% filter(!is.na(Release_Price) & !is.na(Manufacturer)), 
       aes(x = Manufacturer, y = Release_Price, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "GPU Price Distribution by Manufacturer", x = "Manufacturer", y = "Price ($)")

# c) price by manufacturer by year
price_man_year <- data_raw %>%
  filter(!is.na(Release_Price) & !is.na(Manufacturer) & !is.na(Release_Year)) %>%
  group_by(Release_Year, Manufacturer) %>%
  summarise(Avg_Price = mean(Release_Price))

ggplot(price_man_year, aes(x = Release_Year, y = Avg_Price, color = Manufacturer)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "GPU Price Trend by Manufacturer over Years", x = "Year", y = "Avg Price ($)")

# --- 6. Performance by year ---
# --- Memory Speed riêng ---
memory_year <- data_raw %>%
  filter(!is.na(Release_Year)) %>%
  group_by(Release_Year) %>%
  summarise(Avg_Memory = mean(Memory, na.rm = TRUE))

ggplot(memory_year, aes(x = Release_Year, y = Avg_Memory)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Average Memory Speed per Year",
       x = "Year",
       y = "Memory Speed (MHz)")

# --- Pixel Rate riêng ---
pixel_year <- data_raw %>%
  filter(!is.na(Release_Year) & !is.na(Pixel_Rate)) %>%
  group_by(Release_Year) %>%
  summarise(Avg_Pixel_Rate = mean(Pixel_Rate, na.rm = TRUE))

ggplot(pixel_year, aes(x = Release_Year, y = Avg_Pixel_Rate)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue") +
  labs(title = "Average Pixel Rate per Year", x = "Year", y = "Pixel Rate (GPixel/s)")

# --- Boost Clock riêng ---
boost_year <- data_raw %>%
  filter(!is.na(Release_Year) & !is.na(Boost_Clock)) %>%
  group_by(Release_Year) %>%
  summarise(Avg_Boost_Clock = mean(Boost_Clock, na.rm = TRUE))

ggplot(boost_year, aes(x = Release_Year, y = Avg_Boost_Clock)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "blue") +
  labs(title = "Average Boost Clock per Year", x = "Year", y = "Boost Clock (MHz)")

# --- 7. Technical specification by year ---
# Đếm số GPU theo độ phân giải mỗi năm
resolution_year <- data_raw %>%
  filter(!is.na(Release_Year) & !is.na(Best_Resolution)) %>%
  group_by(Release_Year, Best_Resolution) %>%
  summarise(Count = n())

# Biểu đồ bar chart (stacked) cho Resolution theo năm
ggplot(resolution_year, aes(x = Release_Year, y = Count, fill = Best_Resolution)) +
  geom_col() +
  labs(title = "Number of GPUs per Resolution by Year", x = "Year", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# XỬ LÍ MISSING DATA
# 1. Tỉ lệ missing data
library(naniar)
summary_missing <- miss_var_summary(data_raw)
# In hết các dòng
print(summary_missing, n = nrow(summary_missing))

# 2. Kiểm tra xem missing data có random không
library(dplyr)
library(naniar)
### 1. Chuyển tất cả giá trị "" / "NA" / "?" thành NA
data_raw[data_raw == "" | data_raw == "NA" | data_raw == "?"] <- NA
### 2. Loại bỏ các cột toàn NA
data_non_na <- data_raw %>% select(where(~ sum(!is.na(.)) > 0))
### 3. Chỉ giữ các cột numeric hoặc factor (có thể convert sang numeric)
data_numeric <- data_non_na %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  select(where(is.numeric))
### 4. Loại bỏ các cột chỉ có 1 giá trị (không biến động)
data_final <- data_numeric %>% select(where(~ length(unique(na.omit(.))) > 1))
### 5. Chạy MCAR test
random_missing <- mcar_test(data_final)
### 6. Xem kết quả
print(random_missing)
### Quan trọng: Cái này cho ra kết quả dữ liệu khuyết không ngẫu nhiên (not MCAR)
### -> p-value = 0, not MCAR. 
### -> Không thể impute value bằng những phương pháp thông thường
### -> Release_Price missing hơn 80%, nhưng lại quan trọng để trả lời các câu hỏi 
### -> Không đủ cơ sở để lấy 20% để impute 80% missing 
### -> XOÁ Release_Price

# ==========================================
# GPU Analysis: Price & Performance
# ==========================================

# XÉT TẬP PERFORMANCE

# Chọn các biến performance
performance_cols <- c("Boost_Clock", "Core_Speed", "Memory_Bandwidth", 
                      "Memory_Speed", "Pixel_Rate", "Texture_Rate", "ROPs", 
                      "TMUs","Shader")

# ANOVA Pixel Rate trên các tập để chứng minh Pixel Rate đại diện được cho performance

library(dplyr)
library(naniar)

# =========================
# 1. Tập performance
# =========================
performance_cols <- c("Boost_Clock", "Core_Speed", "Memory_Bandwidth", 
                      "Memory_Speed", "Pixel_Rate", "Texture_Rate", "ROPs", 
                      "TMUs", "Shader")

data_perf <- data_raw[performance_cols]

# Xem tỷ lệ missing
cat("=== Missing summary for Performance set ===\n")
print(miss_var_summary(data_perf), n = nrow(miss_var_summary(data_perf)))
dim(data_perf)

# =========================
# 2. Hàm chạy ANOVA cho Pixel_Rate
# =========================
run_anova <- function(df, exclude = NULL, label = "Dataset") {
  predictors <- setdiff(names(df), c("Pixel_Rate", exclude))
  formula <- as.formula(paste("Pixel_Rate ~", paste(predictors, collapse = " + ")))
  
  cat("\n=== ANOVA on", label, "===\n")
  model <- aov(formula, data = df)
  print(summary(model))
}

# =========================
# 3. Chạy các case
# =========================

# Case 1: Đầy đủ predictors
run_anova(data_perf, label = "All predictors")

# Case 2: Bỏ Boost_Clock
run_anova(data_perf, exclude = "Boost_Clock", label = "Exclude Boost_Clock")

# Case 3: Bỏ Core_Speed
run_anova(data_perf, exclude = "Core_Speed", label = "Exclude Core_Speed")

# Kết luận: cả 3 case đều cho thấy là Pixel_rate đều tương quan đáng kể so với những biến còn lại dù là trên tập đầy đủ biến/ subset của nó
# -> có thể dùng Pixel_rate để đại diện cho tập performance


# XÉT TẬP TECHINICAL

# Chọn các biến technical
technical_cols <- c("Architecture", "Best_Resolution", "DVI_Connection", "Dedicated", "Direct_X",
                    "DisplayPort_Connection", "HDMI_Connection", "Integrated", "L2_Cache", "Max_Power",
                    "Memory", "Memory_Bus", "Memory_Type", "Notebook_GPU", "Open_GL", "PSU", "Power_Connector",
                    "Process", "Resolution_WxH", "SLI_Crossfire", "VGA_Connection")

data_tech <- data_raw[technical_cols]

# Xem tỷ lệ missing
cat("=== Missing summary for Technical Specifications set ===\n")
print(miss_var_summary(data_tech), n = nrow(miss_var_summary(data_tech)))
dim(data_tech)

library(dplyr)

# Vì DisplayPort_Connection bị missing lên tới 74%, nên cân nhắc về việc loại bỏ.
# =========================
# Lọc tập con
# =========================
data_DP_price <- data_raw[, c("DisplayPort_Connection", "Release_Price")]
data_DP_price <- data_DP_price[!is.na(data_DP_price$DisplayPort_Connection) & 
                                 !is.na(data_DP_price$Release_Price), ]

data_DP_pixel <- data_raw[, c("DisplayPort_Connection", "Pixel_Rate")]
data_DP_pixel <- data_DP_pixel[!is.na(data_DP_pixel$DisplayPort_Connection) & 
                                 !is.na(data_DP_pixel$Pixel_Rate), ]

# =========================
# Chạy Linear Regression
# =========================
# 1. DisplayPort_Connection ~ Release_Price
lmDP_price <- lm(Release_Price ~ DisplayPort_Connection, data = data_DP_price)
summary(lmDP_price)

# 2. DisplayPort_Connection ~ Pixel_Rate
lmDP_pixel <- lm(Pixel_Rate ~ DisplayPort_Connection, data = data_DP_pixel)
summary(lmDP_pixel)

# Kết luận: chạy Linear Regression cho thấy, DisplayPort_Connection không quan hệ tuyến tính so với 
# Release_Price(~12%) và Pixel_Rate(~40%). 

data_tech <- data_tech[, !names(data_tech) %in% "DisplayPort_Connection"]

library(dplyr)
library(tidyr)
library(stringr)

dim(data_tech)
head(data_tech)

# Trước khi impute, cần xem xét và reprocessing một số biến factor

library(dplyr)
library(tidyr)
library(stringr)

# 1. Best Resolution & Resolution_WxH
library(dplyr)
library(tidyr)
library(stringr)

# Hàm tách resolution và tính tổng pixel
process_resolution <- function(data, col_name) {
  col_name <- enquo(col_name)  # để dùng với dplyr
  new_width <- paste0(as_label(col_name), "_Width")
  new_height <- paste0(as_label(col_name), "_Height")
  total_pixel <- paste0(as_label(col_name), "_TotalPixels")
  
  data %>%
    separate(!!col_name, into = c(new_width, new_height), sep = "x", fill = "right", remove = FALSE) %>%
    mutate(
      !!new_width := as.numeric(str_trim(.data[[new_width]])),
      !!new_height := as.numeric(str_trim(.data[[new_height]])),
      !!total_pixel := .data[[new_width]] * .data[[new_height]]
    )
}

# Ví dụ dùng cho Best_Resolution
data_tech <- process_resolution(data_tech, Best_Resolution)

# Ví dụ dùng cho Resolution_WxH
data_tech <- process_resolution(data_tech, Resolution_WxH)

# Xem frequency Total Pixels của Best_Resolution
pixel_freq <- data_tech %>%
  count(Best_Resolution_TotalPixels) %>%
  arrange(desc(n))

print(pixel_freq)

# Xem frequency Total Pixels của Resolution_WxH
pixel_freq <- data_tech %>%
  count(Resolution_WxH_TotalPixels) %>%
  arrange(desc(n))

print(pixel_freq)

data_tech <- data_tech %>%
  mutate(Pixel_Level_Best = case_when(
    Best_Resolution_TotalPixels < 1050000 ~ "Low",
    Best_Resolution_TotalPixels <= 2073600 ~ "Medium-Low",  # Full HD đỉnh cao
    Best_Resolution_TotalPixels <= 4096000 ~ "Medium",
    Best_Resolution_TotalPixels <= 8300000 ~ "High",
    Best_Resolution_TotalPixels > 8300000 ~ "Very High",
    TRUE ~ NA_character_
  ))

data_tech <- data_tech %>%
  mutate(Pixel_Level = case_when(
    Resolution_WxH_TotalPixels < 2100000 ~ "Low",
    Resolution_WxH_TotalPixels < 4100000 ~ "Medium-Low",
    Resolution_WxH_TotalPixels < 9000000 ~ "Medium",
    Resolution_WxH_TotalPixels < 16000000 ~ "High",
    Resolution_WxH_TotalPixels >= 16000000 ~ "Very High",
    TRUE ~ NA_character_
  ))

data_tech <- data_tech[, !(names(data_tech) %in%  c("Best_Resolution","Best_Resolution_Width", "Best_Resolution_Height"))]
data_tech <- data_tech[, !(names(data_tech) %in%  c("Res_Width", "Res_Height"))]
data_tech <- data_tech[, !(names(data_tech) %in%  c("Resolution_WxH", "Resolution_WxH_Width", "Resolution_WxH_Height"))]
data_tech <- data_tech[, !(names(data_tech) %in%  c("Resolution_WxH_TotalPixels", "Best_Resolution_TotalPixels"))]
data_tech <- data_tech[, !(names(data_tech) %in%  c("Total_Pixels"))]


# 2. Các biến connection: DVI, HDMI, VGA, chuyển sang binary để giảm biến dummy về sau
library(dplyr)
data_tech <- data_tech %>%
  mutate(
    # 1 nếu card có cổng, NA nếu missing, 0 nếu thật sự không có cổng
    Has_HDMI = case_when(
      !is.na(HDMI_Connection) & HDMI_Connection > 0 ~ 1,
      !is.na(HDMI_Connection) & HDMI_Connection == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    Has_DVI = case_when(
      !is.na(DVI_Connection) & DVI_Connection > 0 ~ 1,
      !is.na(DVI_Connection) & DVI_Connection == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    Has_VGA = case_when(
      !is.na(VGA_Connection) & VGA_Connection > 0 ~ 1,
      !is.na(VGA_Connection) & VGA_Connection == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

data_tech <- data_tech[, !(names(data_tech) %in%  c("VGA_Connection", "DVI_Connection", "HDMI_Connection"))]

# 3. Architecture: chỉ lấy phần tên trước khoảng cách
data_tech$Architecture_simple <- sub(" .*", "", data_tech$Architecture)
data_tech <- data_tech[, !(names(data_tech) %in%  c("Architecture"))]

table(data_tech$Architecture_simple)

# Tuy nhiên nhận thấy dùng đã lọc ra chữ cái đầu (vd: "RV740", "RS780D", ...) thì số lượng levels
# được tạo ra cũng lên đến 230 biến -> quá nhiều biến dummy -> nên LOẠI HẲN
# Đồng thời nhận thấy các biến như: Manufacturer và Release_Date có thể giải thích được biến architecture
# -> LOẠI

data_tech <- data_tech[, !(names(data_tech) %in%  c("Architecture_simple"))]

# 4. PSU: Tách Watt và Amp
data_tech <- data_tech %>%
  separate(PSU, into = c("PSU_Watt", "PSU_Amps"), sep = "&", fill = "right", remove = FALSE) %>%
  mutate(
    PSU_Watt = as.numeric(str_trim(str_remove(PSU_Watt, "Watt"))),
    PSU_Amps = as.numeric(str_trim(str_remove(PSU_Amps, "Amps")))
  )
data_tech <- data_tech[, !(names(data_tech) %in%  c("PSU"))]

head(data_tech)
dim(data_tech)
str(data_tech)

# Chuyển các cột Yes/No hoặc categorical về factor
cols_factor <- c("Dedicated", "Integrated", "Notebook_GPU", "SLI_Crossfire",
                 "Has_HDMI", "Has_DVI", "Has_VGA", "Power_Connector","Memory_Type","Pixel_Level_Best","Pixel_Level")

data_tech[cols_factor] <- lapply(data_tech[cols_factor], as.factor)

# Bỏ cột factor chỉ có 1 level (nếu có)
one_level <- sapply(data_tech[factor_cols], function(x) nlevels(x) == 1)
data_tech <- data_tech[, !names(data_tech) %in% names(one_level[one_level])]

# Kiểm tra lại kiểu dữ liệu
str(data_tech)

# Chạy MICE
library(mice)
set.seed(123)

# Chạy MICE với 5 imputations, 10 vòng lặp
imputed_mice <- mice(data_tech, m = 5, maxit = 10, method = "rf", seed = 123)

# Lấy dataset đã impute (lấy bộ thứ 1)
data_tech_imputed <- complete(imputed_mice, 1)


# Xem kết quả
summary(data_tech_imputed)

# Ghi dữ liệu đã impute ra file CSV
write.csv(
  data_tech_imputed,
  file = "datasets/All_GPUs_technical_imputed.csv",
  row.names = FALSE
)
head(data_tech_imputed)
head(data_raw)
dim(data_raw)
dim(data_tech_imputed)
str(data_tech_imputed)


library(mice)
library(dplyr)

set.seed(123)

# ------------------------------
# 1. Xác định loại biến
# ------------------------------
numeric_vars <- names(data_tech)[sapply(data_tech, is.numeric)]
factor_vars <- names(data_tech)[sapply(data_tech, is.factor)]

# ------------------------------
# 2. Chọn phương pháp cho từng biến
# ------------------------------
methods <- make.method(data_tech)
methods[numeric_vars] <- "pmm"        # numeric -> predictive mean matching
methods[factor_vars] <- "polyreg"     # factor -> polytomous logistic

# ------------------------------
# 3. Chạy MICE
# ------------------------------
imputed_mice <- mice(data_tech, m = 5, maxit = 10, method = methods, seed = 123)

# ------------------------------
# 4. Lấy dataset đã impute
# ------------------------------
data_tech_imputed <- complete(imputed_mice, 1)

# ------------------------------
# 5. Giới hạn outlier cho numeric (ví dụ 1%-99% percentile)
# ------------------------------
for (var in numeric_vars) {
  q <- quantile(data_tech_imputed[[var]], probs = c(0.01, 0.99), na.rm = TRUE)
  data_tech_imputed[[var]] <- pmin(pmax(data_tech_imputed[[var]], q[1]), q[2])
}

# ------------------------------
# 6. Kiểm tra kết quả
# ------------------------------
summary(data_tech_imputed)
str(data_tech_imputed)
head(data_tech_imputed)

# ------------------------------
# 7. Lưu CSV
# ------------------------------
write.csv(
  data_tech_imputed,
  file = "datasets/All_GPUs_technical_imputed_clean.csv",
  row.names = FALSE
)

# Gộp data lại (2 data cùng số dòng), cột nào trùng tên thì lấy của imputed

# Tìm tên cột trùng
dup_cols <- intersect(names(data_raw), names(data_tech_imputed))

# Loại các cột trùng bên data_raw, sau đó cbind
data_combined <- cbind(
  data_raw[ , !(names(data_raw) %in% dup_cols)],
  data_tech_imputed
)
data_combined <- data_combined[, !(names(data_combined) %in%  c("Resolution_WxH","Best_Resolution","PSU","DVI_Connection","VGA_Connection","HDMI_Connection"))]

dim(data_combined)
str(data_combined)


# I. Predict GPU Price based on technical specifications:

# ------------------------------------------
# 1.Lọc dữ liệu: chỉ giữ những data có Release_Price không NA
# ------------------------------------------
data_price <- data_combined %>%
  filter(!is.na(Release_Price))

# Kiểm tra dữ liệu
cat("Number of GPUs with price info:", nrow(data_price), "\n")
summary(data_price$Release_Price)

# --- Descriptive Statistics ---
library(ggplot2)
library(dplyr)

### 1.Tổng quan Release Price
summary(data_price$Release_Price)  # min, 1st Qu., median, mean, 3rd Qu., max
sd(data_price$Release_Price, na.rm = TRUE)  # độ lệch chuẩn
quantile(data_price$Release_Price, probs = seq(0,1,0.1))  # phân vị 0-100%

### 2.Phân bố theo năm phát hành
year_count <- data_price %>%
  group_by(Release_Year) %>%
  summarise(Count = n(),
            Avg_Price = mean(Release_Price, na.rm = TRUE),
            Median_Price = median(Release_Price, na.rm = TRUE))

print(year_count)

### 3.Visualization
# a) Histogram của Release_Price
ggplot(data_price, aes(x = Release_Price)) +
  geom_histogram(binwidth = 300, fill = "skyblue", color = "black") +
  labs(title = "Distribution of GPU Release Price",
       x = "Release Price", y = "Count")

# b) Boxplot theo Release_Year để thấy phân bố giá theo năm
ggplot(data_price, aes(x = factor(Release_Year), y = Release_Price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "GPU Release Price Distribution by Year",
       x = "Release Year", y = "Release Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# c) Line plot trung bình giá theo năm
ggplot(year_count, aes(x = Release_Year, y = Avg_Price)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average GPU Release Price Over Years",
       x = "Release Year", y = "Average Price")

# Loại bỏ outlier có Release_Price = 14999
data_price_clean <- data_price %>%
  filter(Release_Price != 14999)

# Kiểm tra lại dữ liệu
summary(data_price_clean$Release_Price)
nrow(data_price_clean)
dim(data_price)
# Vẽ histogram sau khi loại bỏ outlier
ggplot(data_price_clean, aes(x = Release_Price)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  labs(title = "Distribution of GPU Release Price (Outlier Removed)",
       x = "Release Price", y = "Count")

# Tỉ lệ missing data
library(naniar)
summary_missing <- miss_var_summary(data_price_clean)
# In hết các dòng
print(summary_missing, n = nrow(summary_missing))

# ------------------------------------------
# 2.Split dữ liệu thành Training và Validation
# ------------------------------------------

library(caret)
library(car)       # để check VIF
library(MASS)      # stepAIC
library(dplyr)

set.seed(2025)
train_index <- createDataPartition(data_price_clean$Release_Price, p = 0.8, list = FALSE)
train_data <- data_price_clean[train_index, ]
valid_data <- data_price_clean[-train_index, ]


# ------------------------------------------
# 3. Fit mô hình đầy đủ (full model) với các technical specs
# ------------------------------------------

full_formula <- Release_Price ~ Dedicated + Direct_X + Integrated + 
  L2_Cache + Max_Power + Memory + Memory_Bus + Memory_Type + 
  Notebook_GPU + Open_GL + PSU_Watt + PSU_Amps + Power_Connector + 
  Process + SLI_Crossfire + Pixel_Level_Best + Pixel_Level + 
  Has_HDMI + Has_DVI + Has_VGA
# Xoá NA
train_data_clean <- na.omit(train_data)

# Drop unused factor levels
train_data_clean <- droplevels(train_data_clean)

# Tìm các biến factor chỉ còn 1 level
drop_vars <- names(train_data_clean)[sapply(train_data_clean, function(x) is.factor(x) && nlevels(x) < 2)]
print(drop_vars)  # in ra các biến sẽ bị loại bỏ

# Tạo công thức mới, bỏ các biến này
formula_vars <- setdiff(all.vars(full_formula), drop_vars)
clean_formula <- as.formula(paste("Release_Price ~", paste(formula_vars[-1], collapse = " + ")))

# Fit model
full_model <- lm(clean_formula, data = train_data_clean)
summary(full_model)

# ------------------------------------------
# 4. Kiểm tra đa cộng tuyến (VIF)
# ------------------------------------------
vif(full_model)

# Nếu có biến VIF quá cao (thường > 5 hoặc 10), cần loại dần.

# ------------------------------------------
# 5. Chọn mô hình tốt nhất bằng AIC
# ------------------------------------------

step_aic <- stepAIC(full_model, direction = "both", trace = FALSE)

# ------------------------------------------
# 6. Chọn mô hình tốt nhất bằng BIC (k = log(n))
# ------------------------------------------

n <- nrow(train_data)
step_bic <- stepAIC(full_model, direction = "both", k = log(n), trace = FALSE)

# ------------------------------------------
# Summary mô hình
# ------------------------------------------
summary(full_model)
summary(step_aic)
summary(step_bic)

# ------------------------------------------
# 8. Chọn mô hình tốt nhất bằng BIC (k = log(n))
# ------------------------------------------

valid_data_clean <- droplevels(valid_data)

# Đồng bộ các levels của factor giữa train và validation
factor_vars <- names(train_data_clean)[sapply(train_data_clean, is.factor)]
for(fv in factor_vars){
  valid_data_clean[[fv]] <- factor(valid_data_clean[[fv]], levels = levels(train_data_clean[[fv]]))
}

pred_aic <- predict(step_aic, newdata = valid_data_clean)
pred_bic <- predict(step_bic, newdata = valid_data_clean)

MAE_aic <- mean(abs(pred_aic - valid_data_clean$Release_Price), na.rm=TRUE)
RMSE_aic <- sqrt(mean((pred_aic - valid_data_clean$Release_Price)^2, na.rm=TRUE))

MAE_bic <- mean(abs(pred_bic - valid_data_clean$Release_Price), na.rm=TRUE)
RMSE_bic <- sqrt(mean((pred_bic - valid_data_clean$Release_Price)^2, na.rm=TRUE))

results <- data.frame(
  Model = c("AIC", "BIC"),
  MAE = c(MAE_aic, MAE_bic),
  RMSE = c(RMSE_aic, RMSE_bic)
)
print(results)



# CÂU D)
# I. Predict GPU Performance (Pixel_Rate) based on technical specifications:

# ------------------------------------------
# 1. Lọc dữ liệu: chỉ giữ những data có Pixel_Rate không NA
# ------------------------------------------
data_perf <- data_combined %>%
  filter(!is.na(Pixel_Rate))

cat("Number of GPUs with Pixel_Rate info:", nrow(data_perf), "\n")
summary(data_perf$Pixel_Rate)

# Loại bỏ outlier nếu cần (ví dụ > 2 SD trên mean)
upper_limit <- mean(data_perf$Pixel_Rate) + 2*sd(data_perf$Pixel_Rate)
data_perf_clean <- data_perf %>% filter(Pixel_Rate <= upper_limit)

# ------------------------------------------
# 2. Split dữ liệu thành Training và Validation
# ------------------------------------------
set.seed(2025)
train_index <- createDataPartition(data_perf_clean$Pixel_Rate, p = 0.8, list = FALSE)
train_data <- data_perf_clean[train_index, ]
valid_data <- data_perf_clean[-train_index, ]

# ------------------------------------------
# 3. Fit mô hình đầy đủ (full model) với các technical specs
# ------------------------------------------
full_formula_perf <- Pixel_Rate ~ Dedicated + Direct_X + Integrated + 
  L2_Cache + Max_Power + Memory + Memory_Bus + Memory_Type + 
  Notebook_GPU + Open_GL + PSU_Watt + PSU_Amps + Power_Connector + 
  Process + SLI_Crossfire + Pixel_Level_Best + Pixel_Level + 
  Has_HDMI + Has_DVI + Has_VGA

# Xoá NA
train_data_clean <- na.omit(train_data)
train_data_clean <- droplevels(train_data_clean)

# Loại các factor chỉ còn 1 level
drop_vars <- names(train_data_clean)[sapply(train_data_clean, function(x) is.factor(x) && nlevels(x) < 2)]
formula_vars <- setdiff(all.vars(full_formula_perf), drop_vars)
clean_formula_perf <- as.formula(paste("Pixel_Rate ~", paste(formula_vars[-1], collapse = " + ")))

# Fit full model
full_model_perf <- lm(clean_formula_perf, data = train_data_clean)
summary(full_model_perf)

# ------------------------------------------
# 4. Kiểm tra đa cộng tuyến (VIF)
# ------------------------------------------
vif(full_model_perf)

# ------------------------------------------
# 5. Chọn mô hình tốt nhất bằng AIC
# ------------------------------------------
step_aic_perf <- stepAIC(full_model_perf, direction = "both", trace = FALSE)

# ------------------------------------------
# 6. Chọn mô hình tốt nhất bằng BIC
# ------------------------------------------
n <- nrow(train_data_clean)
step_bic_perf <- stepAIC(full_model_perf, direction = "both", k = log(n), trace = FALSE)

# ------------------------------------------
# 7. Chuẩn bị validation set
# ------------------------------------------
valid_data_clean <- droplevels(valid_data)

# Đồng bộ các levels của factor giữa train và validation
factor_vars <- names(train_data_clean)[sapply(train_data_clean, is.factor)]
for(fv in factor_vars){
  valid_data_clean[[fv]] <- factor(valid_data_clean[[fv]], levels = levels(train_data_clean[[fv]]))
}

# ------------------------------------------
# 8. Predict và tính MAE, RMSE
# ------------------------------------------
pred_aic <- predict(step_aic_perf, newdata = valid_data_clean)
pred_bic <- predict(step_bic_perf, newdata = valid_data_clean)

MAE_aic <- mean(abs(pred_aic - valid_data_clean$Pixel_Rate), na.rm = TRUE)
RMSE_aic <- sqrt(mean((pred_aic - valid_data_clean$Pixel_Rate)^2, na.rm = TRUE))

MAE_bic <- mean(abs(pred_bic - valid_data_clean$Pixel_Rate), na.rm = TRUE)
RMSE_bic <- sqrt(mean((pred_bic - valid_data_clean$Pixel_Rate)^2, na.rm = TRUE))

results_perf <- data.frame(
  Model = c("AIC", "BIC"),
  MAE = c(MAE_aic, MAE_bic),
  RMSE = c(RMSE_aic, RMSE_bic)
)
print(results_perf)


# -------------------------------
# ii. Performance-to-price ratio over time
# -------------------------------
data_price <- data_combined %>%
  filter(!is.na(Release_Price))

## Vẽ performance-to-price ratio over time
library(dplyr)
library(ggplot2)

# Tính Performance-to-Price Ratio
data_price <- data_price %>%
  mutate(Perf_Price_Ratio = Pixel_Rate / Release_Price)

# Tổng hợp theo release_quarter
perf_price_qtr <- data_price %>%
  group_by(Release_Quarter) %>%
  summarise(Avg_Ratio = mean(Perf_Price_Ratio, na.rm = TRUE)) %>%
  arrange(Release_Quarter)

# Vẽ trend theo release_quarter
ggplot(perf_price_qtr, aes(x = Release_Quarter, y = Avg_Ratio, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Performance-to-Price Ratio Over Time (by Quarter)",
       x = "Release Quarter", y = "Average Pixel Rate / Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------
# 3. Chạy ANOVA
# ------------------------------
anova_full_qtr <- aov(Perf_Price_Ratio ~ Release_Quarter, data = data_price)
summary(anova_full_qtr)


# Kiểm tra trend chỉ xét quý

library(dplyr)
library(ggplot2)

# ------------------------------
# 1. Tách quý từ Release_Quarter
# Giả sử Release_Quarter dạng "2023 Q1"
# ------------------------------
data_price <- data_price %>%
  mutate(Quarter = sub(".* (Q[1-4])", "\\1", Release_Quarter))

# Chuyển thành factor theo thứ tự Q1-Q4
data_price$Quarter <- factor(data_price$Quarter, levels = c("Q1", "Q2", "Q3", "Q4"))

# ------------------------------
# 2. Vẽ boxplot theo quý
# ------------------------------
ggplot(data_price, aes(x = Quarter, y = Perf_Price_Ratio)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Performance-to-Price Ratio by Quarter",
       x = "Quarter", y = "Pixel Rate / Price") +
  theme_minimal()

# ------------------------------
# 4. Chạy ANOVA để kiểm tra ý nghĩa
# ------------------------------
anova_quarter <- aov(Perf_Price_Ratio ~ Quarter, data = data_price)
summary(anova_quarter)
TukeyHSD(anova_quarter)


# -------------------------------
# iii. Are there any manufacturers that stand out in specific performance and price segments?
# -------------------------------
# Price Segment
# 1. Lọc dữ liệu chỉ giữ GPUs có giá
data_price <- data_combined %>%
  filter(!is.na(Release_Price))

# 2. Tóm tắt thông tin giá theo Manufacturer
manufacturer_summary <- data_price %>%
  group_by(Manufacturer) %>%
  summarise(
    Avg_Price = mean(Release_Price, na.rm = TRUE)  # Giá trung bình
    , Median_Price = median(Release_Price, na.rm = TRUE)  # Giá trung vị
    , Max_Price = max(Release_Price, na.rm = TRUE)  # Giá cao nhất
    , Min_Price = min(Release_Price, na.rm = TRUE)  # Giá thấp nhất
    , Count = n()  # Số GPU
    , .groups = "drop"
  ) %>%
  arrange(desc(Avg_Price))  # Sắp xếp theo giá trung bình giảm dần

# 3. Line plot: trung bình giá theo Manufacturer
ggplot(data_price, aes(x = Manufacturer, y = Release_Price)) +
  geom_jitter(aes(color = Manufacturer), width = 0.2, alpha = 0.7, size = 2) +
  labs(title = "GPU Release Price by Manufacturer (Outliers Reduced)",
       x = "Manufacturer",
       y = "Release Price") +
  coord_cartesian(ylim = c(0, 2000)) +  # giới hạn y để loại bớt outliers
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Boxplot giá theo Manufacturer
ggplot(data_combined, aes(x = Manufacturer, y = Release_Price, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "Distribution of GPU Release Price by Manufacturer"
       , x = "Manufacturer"
       , y = "Release Price") +
  coord_cartesian(ylim = c(0, 2000)) +  # Chỉ tập trung vào giá phổ biến
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Chia Price Segment (Low, Mid, High)
data_combined <- data_combined %>%
  mutate(Price_Segment = cut(Release_Price
                             , breaks = quantile(Release_Price, probs = seq(0,1,0.33), na.rm = TRUE)
                             , include.lowest = TRUE
                             , labels = c("Low", "Mid", "High")))

# 6. Tóm tắt performance theo Manufacturer và Price Segment
segment_summary <- data_combined %>%
  group_by(Manufacturer, Price_Segment) %>%
  summarise(
    Avg_Performance = mean(Pixel_Rate, na.rm = TRUE)  # Trung bình performance
    , Max_Performance = max(Pixel_Rate, na.rm = TRUE)  # Max performance
    , Count = n()  # Số GPU
    , .groups = "drop"
  )

segment_summary
# 7. Boxplot Performance theo Price Segment
ggplot(data_combined %>% filter(!is.na(Price_Segment))
       , aes(x = Price_Segment, y = Pixel_Rate, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "GPU Performance by Manufacturer and Price Segment"
       , x = "Price Segment"
       , y = "Pixel Rate") +
  theme_minimal()


# PERFORMANCE (chỉ xét Pixel_Rate)
# Performance Segment
# 1. Lọc dữ liệu chỉ giữ GPUs có Pixel_Rate
data_perf <- data_combined %>%
  filter(!is.na(Pixel_Rate))

# 2. Tóm tắt thông tin performance theo Manufacturer
manufacturer_perf_summary <- data_perf %>%
  group_by(Manufacturer) %>%
  summarise(
    Avg_Performance = mean(Pixel_Rate, na.rm = TRUE)  # Performance trung bình
    , Median_Performance = median(Pixel_Rate, na.rm = TRUE)  # Performance trung vị
    , Max_Performance = max(Pixel_Rate, na.rm = TRUE)  # Max Performance
    , Min_Performance = min(Pixel_Rate, na.rm = TRUE)  # Min Performance
    , Count = n()  # Số GPU
    , .groups = "drop"
  ) %>%
  arrange(desc(Avg_Performance))  # Sắp xếp theo performance giảm dần

# 3. Scatter plot Performance vs Manufacturer
ggplot(data_perf, aes(x = Manufacturer, y = Pixel_Rate)) +
  geom_jitter(aes(color = Manufacturer), width = 0.2, alpha = 0.7, size = 2) +
  labs(title = "GPU Performance by Manufacturer (Outliers Reduced)",
       x = "Manufacturer",
       y = "Pixel Rate") +
  coord_cartesian(ylim = c(0, 150)) +  # Giới hạn y để giảm outliers
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Boxplot Performance theo Manufacturer
ggplot(data_perf, aes(x = Manufacturer, y = Pixel_Rate, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "Distribution of GPU Performance by Manufacturer",
       x = "Manufacturer",
       y = "Pixel Rate") +
  coord_cartesian(ylim = c(0, 150)) +  # Chỉ tập trung vào phần phổ biến
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Chia Performance Segment (Low, Mid, High)
data_combined <- data_combined %>%
  mutate(Performance_Segment = cut(Pixel_Rate,
                                   breaks = quantile(Pixel_Rate, probs = seq(0,1,0.33), na.rm = TRUE),
                                   include.lowest = TRUE,
                                   labels = c("Low", "Mid", "High")))

# 6. Tóm tắt performance theo Manufacturer và Performance Segment
perf_segment_summary <- data_combined %>%
  filter(!is.na(Performance_Segment)) %>%
  group_by(Manufacturer, Performance_Segment) %>%
  filter(!all(is.na(Release_Price))) %>%  # chỉ giữ nhóm có ít nhất 1 giá
  summarise(
    Avg_Price = mean(Release_Price, na.rm = TRUE),
    Max_Price = max(Release_Price, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  )

perf_segment_summary
# 7. Boxplot Price theo Performance Segment
ggplot(data_combined %>% filter(!is.na(Performance_Segment)),
       aes(x = Performance_Segment, y = Release_Price, fill = Manufacturer)) +
  geom_boxplot() +
  labs(title = "GPU Release Price by Manufacturer and Performance Segment",
       x = "Performance Segment",
       y = "Release Price") +
  coord_cartesian(ylim = c(0, 2000)) +  # Giới hạn giá để tránh outliers
  theme_minimal()
