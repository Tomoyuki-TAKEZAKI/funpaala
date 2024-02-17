maybe0 (Just x) = x
maybe0 Nothing = 0

nothingness Nothing = True
nothingness (Just _) = False

safeRecip 0 = Nothing
safeRecip x = Just (1 / x)

helloTo "Takezaki" = "Good morning, sir."
helloTo n = "Hello, " ++ n ++ "!"

friend (Just name) = name ++ " is my friend."
friend Nothing = "I'm alone."

notZero 0 = Nothing
notZero x = Just x

div3 x
    | x `mod` 3 == 0 = x `div` 3
    | otherwise = x