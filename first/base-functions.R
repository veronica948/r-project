#with default step = 1
#arr = 1:12
#arr[5]
# with step = 0.25
#s = seq(1,2,0.25)
#s[3]
#list
#list1 = c(1,3,8)
#list1[1]
#average
#mean(arr)
#sd(arr)
#var(arr)
#median(seq(1,2,0.25))
n = 1000:3500
for(n1 in n) {
  for(i in 1:20) {
  s1 = runif(n1, 0, 1)
  avg = mean(s1)
  print(avg)
  cor = (abs(0.5 - avg) < 0.01)
  if(cor == FALSE) {
    print(n1)
    print('bad')
    break()
  }
  }
  if(cor == TRUE) {
    print(n1)
    print('Ok')
    break()
  }
}


