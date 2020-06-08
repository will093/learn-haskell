module TupleSwap where


tupleSwap :: (a, b) -> (c, d) -> ((b, d), (a, c))
tupleSwap x y = ((snd x, snd y), (fst x, fst y))   