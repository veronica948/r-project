#with default step = 1
arr = 1:12
arr[5]
# with step = 0.25
s = seq(1,2,0.25)
s[3]
#list
list1 = c(1,3,8)
list1[1]
#average
mean(arr)
sd(arr)
var(arr)
median(seq(1,2,0.25))
n = 2900:3100
for(n1 in n) {
  s1 = runif(n1, 0, 1)
  avg = mean(s1)
  print(n1)
  print(avg)
  cor = (abs(0.5 - avg) < 0.01)
  print(cor)
}

