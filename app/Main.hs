module Main where

    func :: String -> [Char]
    func a = a

    main :: IO ()
    main = do
        putStrLn "Please enter your name:"
        name <- func "sex"
        putStrLn ("Hello, " ++ name ++ ", how are you?")
        
