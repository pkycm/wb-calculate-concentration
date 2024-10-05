# 加载必要的R库
library(dplyr)
library(ggplot2)

# 1. 定义示例输入数据
data <- data.frame(
  Concentration_or_Sample = c(1, 0.5, 0.25, 0.125, 0.0625, 0.03125, 0, 
                              "C-1(WT/6w)", "C-2(WT/6w)", "C-3(WT/6w)",
                              "V-1(WT/6w)", "V-2(WT/6w)", "V-3(WT/6w)", 
                              "V-5(LM/7w)", "G-1(WT/6w)", "G-2(WT/6w)", "G-3(WT/6w)"),
  Absorbance_1 = c(1.327, 0.785, 0.547, 0.398, 0.311, 0.281, 0.236,
                   0.364, 0.349, 0.322, 0.332, 0.321, 0.362, 0.46, 0.342, 0.292, 0.359),
  Absorbance_2 = c(1.319, 0.828, 0.549, 0.385, 0.312, 0.272, 0.233,
                   0.388, 0.349, 0.322, 0.336, 0.314, 0.342, 0.458, 0.329, 0.29, 0.345),
  Original_Volume = c(NA, NA, NA, NA, NA, NA, NA,
                      150, 150, 150, 150, 150, 150, 150, 150, 150, 150)
)
#或者直接读取以上dataframe形式的excel文件（可选）
#data <- read.table("clipboard", header = TRUE, sep = "\t")
# 2. 计算标准品和样品的平均吸光度
data <- data %>% 
  mutate(Mean_Absorbance = rowMeans(select(., Absorbance_1, Absorbance_2), na.rm = TRUE))

# 3. 提取标准品数据（将浓度列转换为数值类型）
standard_data <- data[1:7, ] %>%
  mutate(Concentration_or_Sample = as.numeric(as.character(Concentration_or_Sample)))

# 4. 创建标准曲线模型（线性回归）
standard_curve <- lm(Concentration_or_Sample ~ Mean_Absorbance, data = standard_data)

# 5. 提取回归方程的系数和R²值
eq <- paste0("y = ", round(coef(standard_curve)[2], 4), "x + ", round(coef(standard_curve)[1], 4))
r_squared <- round(summary(standard_curve)$r.squared, 4)

# 6. 绘制标准曲线图并添加回归方程和R²
ggplot(standard_data, aes(x = Mean_Absorbance, y = Concentration_or_Sample)) +
  geom_point(color = "blue", size = 3) +  # 散点图
  geom_smooth(method = "lm", formula = y ~ x, color = "red", linetype = "dashed", size = 1) +  # 添加回归线
  labs(title = "Standard Curve Linear Regression",
       x = "Mean Absorbance",
       y = "Concentration (µg/µL)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  ) +
  # 确保 max() 仅在数值列上操作
  annotate("text", x = 0.5, y = max(as.numeric(standard_data$Concentration_or_Sample)) * 0.8,  
           label = paste(eq, "\nR² = ", r_squared),
           color = "black", size = 5, hjust = 0)
# 5. 提取样品数据
sample_data <- data[8:nrow(data), ]

# 6. 计算样品浓度（使用线性模型预测浓度）
sample_data <- sample_data %>% 
  mutate(Concentration = predict(standard_curve, newdata = sample_data))
sample_data$original_concentration <- sample_data$Concentration * 10
# 7. 假设目标浓度为样品中最低浓度值（可自定义）
target_concentration <- min(sample_data$original_concentration, na.rm = TRUE)

# 8. 计算稀释后目标体积（目标浓度下需要的体积）
sample_data <- sample_data %>%
  mutate(Standardized_Volume = (original_concentration * Original_Volume) / target_concentration)

# 9. 计算需要加入的RIPA体积
sample_data <- sample_data %>%
  mutate(RIPA_Volume = Standardized_Volume - Original_Volume)

# 10. 计算需要加入的5× loading buffer体积（目标体积的25%）
sample_data <- sample_data %>%
  mutate(Loading_Buffer_Volume = Standardized_Volume * 0.25)

# 11. 计算最终体积（标准体积 + loading buffer 体积）
sample_data <- sample_data %>%
  mutate(Final_Volume = Standardized_Volume + Loading_Buffer_Volume)

# 12. 打印并保存结果
print(sample_data)

# 如果需要保存为CSV文件，可以使用以下命令：

write.csv(data, "wb.csv", row.names = FALSE)
