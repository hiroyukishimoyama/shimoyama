#データ入力
a = rnorm(20, mean=10, sd=2)

#標本自己共分散
gamma0 = (length(a)-1)/length(a)*var(a)
gamma1 = 0
for(i in 2:length(a)){
  gamma1 = (a[[i]]-mean(a))*(a[[i-1]]-mean(a))
}
gamma1 = gamma1/length(a)

#標本コレログラム
ro0 = gamma1/gamma0

#モデルのパラメータ推定
fai1 <- ro0
fai0 <- mean(a)*(1-fai1)

#3つ先までの推定値を導出
est1 = fai0+fai1*a[[length(a)]]
est2 = fai0+fai1*est1
est3 = fai0+fai1*est2

#結合してプロット
plot(c(a,est1,est2,est3))

