# Generator with Guards

   [ a | a <- [1..3] ]
-- [1,2,3]

   [ (a,b) | a <- [1..3], b <- ["A","B"] ]
-- [(1,"A"),(1,"B"),(2,"A"),(2,"B"),(3,"A"),(3,"B")]

   [ (x,z) | x <- [1..3], z <- [True,False], True ]
-- [(1,True),(1,False),(2,True),(2,False),(3,True),(3,False)] 

   [ (x,z) | x <- [1..3], z <- [True,False], False ]
-- []

   [ (x,y) | x <- [1..3], y <- [True,False], y ]
-- [(1,True),(2,True),(3,True)]



