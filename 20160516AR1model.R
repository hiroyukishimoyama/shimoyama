#�f�[�^����
a = rnorm(20, mean=10, sd=2)

#�W�{���ȋ����U
gamma0 = (length(a)-1)/length(a)*var(a)
gamma1 = 0
for(i in 2:length(a)){
  gamma1 = (a[[i]]-mean(a))*(a[[i-1]]-mean(a))
}
gamma1 = gamma1/length(a)

#�W�{�R�����O����
ro0 = gamma1/gamma0

#���f���̃p�����[�^����
fai1 <- ro0
fai0 <- mean(a)*(1-fai1)

#3��܂ł̐���l�𓱏o
est1 = fai0+fai1*a[[length(a)]]
est2 = fai0+fai1*est1
est3 = fai0+fai1*est2

#�������ăv���b�g
plot(c(a,est1,est2,est3))
