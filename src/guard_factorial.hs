factorial :: Integer -> Integer 
factorial n | n == 0 = 1 
            | n /= 0 = n * factorial (n-1) 

iff :: Bool -> a -> a -> a
iff True x _ = x
iff False _ y = y

fibonacci :: Integer -> Integer
fibonacci n | n<=0    = 0
            | n-2 > 0 = fibonacci(n-1) + fibonacci(n-2)
            | n-1 > 0 = fibonacci(n-1)
            | n==1    = 1

ipow :: (Integer , Integer) -> Integer
ipow (n, m) | m<1  = 1 
            | m>=1 = n * ipow (n , (m-1))

isItPrime_rec :: ( Integer, Integer, Integer , Double ) -> Bool
isItPrime_rec (n, m, p, lM05) = do
                  if (rem m p)==0 && (p>=2) then 
                      n==2
                  else do 
                      if log( fromInteger p )>lM05 then
                          True
                      else do 
                          isItPrime_rec (n-1, m, p+1, lM05)

isItPrime :: Integer -> Bool
isItPrime n = do
                  if n <= 2 then
                      True
                  else do
                      let p = 1
                      let m = n
                      let lM05 = log( fromInteger m ) * 0.5
                      isItPrime_rec (n, m, p, lM05)

--https://gist.github.com/richardtjornhammar/ef1719ab0dc683c69d5a864cb05c5a90
fibonacciTruth :: Integer -> Bool
fibonacciTruth i = do
    let fi_0 = ipow ( fibonacci(i) , 2 ) 
    let fi_1 = ipow ( fibonacci(i+1) , 2 )
    if fi_0+fi_1 == fibonacci(2*i+1) then
        True
    else do
        False

fermatnumber :: Integer -> Integer
fermatnumber i = 1+ipow( 2, ipow (2, i) )

main = do 
   putStrLn "The factorial of 5 is:"  
   print ( factorial 5 )
   print ( "These are some Fibonacci numbers")
   print ( fibonacci (1), fibonacci(2), fibonacci(3),fibonacci(4),fibonacci(5),fibonacci(6),fibonacci(7),fibonacci(8),fibonacci(9),fibonacci(10),fibonacci(11),fibonacci(12) )
   print ( "The following is true: (fibbonacci number i)^2 + (fibbonacci number i+1)^2 = (fibbonacci number 2*i+1)")
   print ( "One such case ", fibonacci(4),"^2 + ",fibonacci(5),"^2 = ",fibonacci(9) )
   print ( "This is also true",fibonacciTruth (11) )
   print ( "This is also true",fibonacciTruth (18) )

   print ( "The first Fermat numbers are" )
   let i_  = 1
   let f_i = fermatnumber i_
   print ( f_i ," and is it prime?", isItPrime(f_i))

   let i_  = 2
   let f_i = fermatnumber i_
   print ( f_i ," and is it prime?", isItPrime(f_i))

   let i_  = 3
   let f_i = fermatnumber i_
   print ( f_i ," and is it prime?", isItPrime(f_i))

   let i_  = 4
   let f_i = fermatnumber i_
   print ( f_i ," and is it prime?", isItPrime(f_i))

   let i_  = 5
   let f_i = fermatnumber i_
   print ( f_i ," and is it prime?", isItPrime(f_i))

   let i_  = 6
   let f_i = fermatnumber i_
   print ( f_i ," and is it prime?", isItPrime(f_i))


