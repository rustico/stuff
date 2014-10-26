-- qsort
qsort [] = []
qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
      where
        lesser = [y | y <- xs, y < x]
        greater = [y | y <- xs, y > x]

-- qsort r
qsort_r [] = []
qsort_r (x:xs) = qsort_r lesser ++ [x] ++ qsort_r greater
      where
        lesser = [y | y <- xs, y >= x]
        greater = [y | y <- xs, y < x]


double  x = x + x

producto [] = 1
producto (x:xs) = x * producto xs


borrar xs = tail (reverse xs)
