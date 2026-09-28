# Number of regular higher education institutions by province and year.
# 2000-2020: China City Statistical Yearbook, city-level counts summed to provinces.
# 1993-1999: provincial counts (province_college_9398.xlsx).
# Output: data/derived/province_college.csv (Province, Year, no_college)
if (!exists("ROOT")) source("code/00_setup.R")

city <- read_excel(file.path(RAW_PROV, "city_control_00_20.xlsx"))
prov_col <- "省份"; year_col <- "年份"; hei_col <- "普通高等学校（所）全市"
province_data <- city %>% select(where(is.numeric), all_of(c(prov_col, year_col))) %>%
  group_by(across(all_of(c(prov_col, year_col)))) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
province_map <- data.frame(
  Chinese = c("四川省","广东省","湖北省","吉林省","内蒙古自治区","江苏省","黑龙江省","云南省",
              "山东省","新疆维吾尔自治区","山西省","浙江省","北京","福建省","上海市","湖南省",
              "辽宁省","安徽省","天津","河南省","陕西省","广西壮族自治区","河北省","西藏自治区",
              "青海省","甘肃省","江西省","重庆市","贵州省","宁夏回族自治区","海南省"),
  English = c("Sichuan","Guangdong","Hubei","Jilin","Inner Mongolia","Jiangsu","Heilongjiang","Yunnan",
              "Shandong","Xinjiang Uygur Autonomous Region","Shanxi","Zhejiang","Beijing","Fujian","Shanghai","Hunan",
              "Liaoning","Anhui","Tianjin","Henan","Shaanxi","Guangxi Zhuang Autonomous Region","Hebei","Tibet",
              "Qinghai","Gansu","Jiangxi","Chongqing","Guizhou","Ningxia Hui Autonomous","Hainan"), stringsAsFactors = FALSE)
names(province_data)[names(province_data) == prov_col] <- "Chinese"
college_0020 <- province_data %>% left_join(province_map, by = "Chinese") %>%
  transmute(Province = English, Year = .data[[year_col]], no_college = .data[[hei_col]])
p9398 <- read_excel(file.path(RAW_PROV, "province_college_9398.xlsx"))
college_9398 <- data.frame(Province = p9398$Province, Year = p9398[[year_col]], no_college = suppressWarnings(as.numeric(p9398[[hei_col]])))
college <- bind_rows(college_0020, college_9398) %>% mutate(Province = str_to_title(Province), no_college = as.numeric(no_college))
write.csv(college, file.path(DERIVED, "province_college.csv"), row.names = FALSE)
cat("province-year college counts:", nrow(college), "rows\n")
