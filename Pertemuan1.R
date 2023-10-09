#Pertemuan 1
#Buat assign variabel <-
A<-55
B<-44
C<-A+B

print(C)

A<B
A==B
A>=B

A<-c(11,22,33,44,55)
B<-c(11,23,33,45,55)
A==B
A<B
A<30

X<- "dia cantik"
Y<- "tapi boong"
paste(X,",",Y)

D<-188
if(C>D){
  print("C is more than D")
} else{
  print("C is less than D")
}

if(C==99){
  print("C is equal to 99")
}

result<-C&D<=150
print(result)

result<-C|D<=150
print(result)

ex<-list("evi", "nia", "sarah")
for (i in ex) {
  print(i)
}

vec<-c(1:10)
x=7
for (v in vec) {
  if(v>x){
    cat(v,"lebih cuy \n")
  } else if(v==x){
    cat(v,"pas \n")
  } else{
    cat(v,"kurang nih \n")
  }
}

v<-rnorm(30)
for (i in 1:20){
  square<-v[i]*v[i]
  print(square)
}


jurusan<-list("tkj", "tei", "ak", "ap", "pm")
typeof(jurusan)
length(jurusan)
jurusan[1]
jurusan[3] <- "akl"
"tkj" %in% jurusan
print(jurusan)
jurusan<-append(jurusan, "toi", after=1)
jurTeknik<- jurusan[1:2]
jurBismen<- jurusan[3:5]
print(jurTeknik)
print(jurBismen)
jurusan<-c(jurTeknik, jurBismen)
jurusan

v1<-c(1:5)
v1
v2<-c(4,6,7,9,10)
v2

v3<- v1+v2
v3

v3[c(1,2)]
v3[1]
sort(v3)
sort(v3, decreasing=TRUE)

df<-data.frame(training=c("strength","stamina","other"), pulse=c(200,300,500),
               duration=c(60,30,20))
print(df)
summary(df)

df[1]
df["training"]
df$training

row_df<-rbind(df, c("fat", 50, 60))
print(row_df)

col_df<-cbind(df, Steps=c(100,200,300))
print(col_df)

#Function
kelLingkaran<- function(r){
  result<- 2*pi*r
  return(result)
}
kelLingkaran(2)

kelBalok<- function(x,y){
  kel<-2*(x+y)
  return(kel)
}
kelBalok(6,6)

facto<- function(n){
  if(n==0){
    return(1)
  }else{
    return(n*factorial(n-2))
  }
}

facto(89)