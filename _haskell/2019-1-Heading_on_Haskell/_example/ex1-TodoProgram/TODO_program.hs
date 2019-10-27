-- # Example #1 - TODO program 
-- ! FROM : http://www.haskellforall.com/2015/10/basic-haskell-examples.html

-- ? CMD : runghc TODO_program.hs
main :: IO ()
main = do
 -- putStrLn :: String -> IO ()
    putStrLn "Commands:"
    putStrLn "+ <String> - Add a TODO entry"
    putStrLn "- <Int>    - Delete the numbered entry"
    putStrLn "q          - Quit"
    prompt []

-- ? MAPPING WITH MONAD TO MANAGE OUTPUTS
prompt :: [String] -> IO ()
prompt todos = do
    putStrLn ""
    putStrLn "Current TODO list:"
    -- - map   :: (a -> b) -> [a] -> [b]
    -- - mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
    -- - mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
    -- https://www.quora.com/What-is-the-difference-between-map-mapM-and-mapM_-in-Haskell
    mapM_ putTodo (zip [0..] todos)
    -- command :: String
    command <- getLine  -- getLine :: IO String
    interpret command todos
    -- - (<-) : bind     --- Monad
    -- - (->) : morphism --- function, functor
    -- - ( =) : equal    --- value

-- ? USER'S INPUT IS INTERPRETTED // add, del, quit, etc
interpret :: String -> [String] -> IO ()
interpret ('+':' ':todo) todos = prompt (todos ++ [todo])
interpret ('-':' ':num ) todos =
                  -- read :: String -> Int
        case delete (read num) todos of
        Nothing -> do
            putStrLn "No TODO entry matches the given number"
            prompt todos
        Just todos' -> prompt todos'
            -- data Maybe a = Just a | Nothing
            -- For this type, a value was either constructed via Just or Nothing,
            -- there are no other (non-error) possibilities.
            -- https://stackoverflow.com/questions/18808258/what-does-the-just-syntax-mean-in-haskell
interpret "q"            _     = return()
interpret command        todos = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    prompt todos

-- ? ADD TODO LIST
putTodo :: (Int, String) -> IO ()
                           -- show :: Int -> String
putTodo (n, todo) = putStrLn (show n ++ ": " ++ todo)

-- ? DELETE TODO LIST
delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return(a:as')
delete _ []     = Nothing