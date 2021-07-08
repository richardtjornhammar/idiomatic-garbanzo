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


fermatnumber :: Integer -> Integer
fermatnumber i = 1+ipow( 2, ipow (2, i) )

main = do 
   putStrLn "The factorial of 5 is:"  
   print ( factorial 5 )
   print ( fibonacci (10) )
   print ( "The first Fermat numbers are" )
   let i_  = 1
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

   let i_  = 2
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

   let i_  = 3
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

   let i_  = 4
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

   let i_  = 5
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

   let i_  = 6
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

   let i_  = 7
   let f_i = fermatnumber i_
   print ( f_i ," and are they prime?", isItPrime(f_i))

