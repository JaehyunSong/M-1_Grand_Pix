# M-1 グランプリ決勝データ

## Description

|変数名|内容|備考|
|---|---|---|
|ID|||
|No|回||
|Year|開催年||
|Rank|最終順位||
|Name|コンビ名||
|Entry_No|エントリー番号||
|Since|コンビ結成年||
|No_Finals|これまでの決勝進出回数||
|Catchphrase|キャッチフレーズ||
|Final|最終決戦進出有無||
|Order10|決勝における入場順番||
|Order3|最終決戦における入場順番|最終決戦に進出しなかったコンビは欠損値|
|Score10|決勝における評価||
|Score3|最終決戦における評価|最終決戦に進出しなかったコンビは欠損値|
|No_Reviewer|審査委員数||

---

## Source

Wikipedia「[M-1グランプリ](https://ja.wikipedia.org/wiki/M-1%E3%82%B0%E3%83%A9%E3%83%B3%E3%83%97%E3%83%AA)」

---

## Sample Code

```r
library(tidyverse)
library(prediction)

df     <- read_csv("M1_Grand_Pix.csv")
bar_df <- df %>%
  group_by(Order10) %>%
  summarise(Mean_Final = mean(Final))

fit <- glm(Final ~ Order10 + Since + No_Finals, 
           data = df, family = binomial("logit"))

summary(fit)
```

```
Call:
glm(formula = Final ~ Order10 + Since + No_Finals, family = binomial("logit"), 
    data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.5954  -0.8731  -0.5989   1.1249   2.0307  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) 54.84634   62.57057   0.877 0.380730    
Order10      0.24927    0.07193   3.466 0.000529 ***
Since       -0.02863    0.03125  -0.916 0.359711    
No_Finals    0.33850    0.13579   2.493 0.012672 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 198.00  on 158  degrees of freedom
Residual deviance: 178.63  on 155  degrees of freedom
AIC: 186.63

Number of Fisher Scoring iterations: 4
```

```r
fit %>% 
  prediction(at = list(Order10 = 1:10)) %>%
  summary() %>%
  rename("Order" = "at(Order10)") %>%
  ggplot() +
  geom_bar(data = bar_df, aes(x = Order10, y = Mean_Final), 
           stat = "Identity", fill = "gray70") +
  geom_pointrange(aes(x = Order, y = Prediction, 
                      ymin = lower, ymax = upper), size = 1.2) +
  geom_line(aes(x = Order, y = Prediction), size = 1.2) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "出場順番", y = "最終決戦へ進出する確率",
       title = "M-1グランプリ (第1回〜第17回)") +
  theme_minimal(base_family = "HiraKakuProN-W3") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 16))
```

![サンプル](/Figs/M1.png)

---

## 更新履歴

* 2021年12月20日: 第17回 (2021年)のデータを追加
* 2020年12月21年: 公開