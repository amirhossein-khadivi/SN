<html>

<body>



<h2 style={font-family: "XB Niloofar">
Sterling Number
</h2>

<p>

تعداد کل افرازهای یک مجموعه n عضوی به g قسمت یا خوشه را می توانیم با عدد
استرلینگ (نوع دوم) محاسبه می کنیم.

``` r
sterling <- function(n,g){
  
    gfact <- 1
    for(i in 1:g){
       gfact <- i*gfact
    }
    gfact

    x <- (1/gfact)

    b <- 0
    for(w in 1:g){

  
         ifact <- 1
         for(s in 1:w){
         ifact <- s*ifact
         }
  
  
         gifact <- 1
         for(p in 0:(g-w)){
         gifact <- (p+1)*gifact
         }
  
  
         b <- b + ( (gfact/(gifact*ifact)) * (((-1)^(g-w))*(w)^(n))  ) 
    
    }

    m <- x*b
    m

}
```

<br>

تعداد کل افرازهای یک مجموعه 25 عضوی به 10 خوشه با استغاده از تابعی که در
اسکرپبت بالا نوشتیم را محاسبه میکنیم. <br>

``` r
sterling(25,10)
```

    ## [1] 1.911966e+18

<br>

</p>

</html>
