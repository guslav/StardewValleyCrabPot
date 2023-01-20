module Main (main) where

import Data.IORef
import Data.Ratio ((%))
import Data.Char (toLower)

import System.IO (hSetBuffering, BufferMode(..) , stdout)

import StardewValley

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    place   <- newIORef InOcean
    farming <- newIORef NoRecycling
    fishing <- newIORef NoFishing
    sashimi <- newIORef False
    let printcase' = printcase place farming fishing sashimi
    putStrLn "Welcome to the stardew valley crab pot calculator. Type 'help' if you want to see a list of the available commands."
    let main' = do
            p <- readIORef place
            a <- readIORef farming
            i <- readIORef fishing
            s <- readIORef sashimi
            putStr "\nStardewValley> "
            command <- words <$> map toLower <$> getLine
            let tree = getProbabilityTree p i :: ProbTree Rational Item
            let profit = avarageProfit i a s tree
            case command of
                "q":_      -> putStrLn "bye!"
                "quit":_   -> putStrLn "bye!"
                "exit":_   -> putStrLn "bye!"
                "help":_   -> help >> main'
                "printcase":_ -> printcase' >> main'
                "set":p1:l    -> do
                    case p1 of
                        "place" -> case l of
                            ["ocean"]      -> writeIORef place InOcean
                            ["freshwater"] -> writeIORef place InFresh
                            _ -> do
                                putStrLn "Wasn't able to read the place at which you want to fish with your crab pot. Available options are 'ocean' and 'freshwater'."
                                putStrLn "Nothing changed.\n"
                        "farming" -> case l of
                            ["norecycling"] -> writeIORef farming NoRecycling
                            ["no","recycling"] -> writeIORef farming NoRecycling
                            ["nofarming"] -> writeIORef farming NoFarming
                            ["no","farming"] -> writeIORef farming NoFarming
                            ["rancher"] -> writeIORef farming Rancher
                            ["artisan"] -> writeIORef farming Artisan
                            _ -> do
                                putStrLn "Wasn't able to read your farming skill. Available options are 'norecycling', 'nofarming', 'rancher' and 'artisan'."
                                putStrLn "Nothing changed.\n"
                        "fishing" -> case l of
                            ["nofishing"] -> writeIORef fishing NoFishing
                            ["no","fishing"] -> writeIORef fishing NoFishing
                            ["fisher"] -> writeIORef fishing Fisher 
                            ["angler"] -> writeIORef fishing Angler 
                            ["mariner"] -> writeIORef fishing Mariner 
                            ["marinerangler"] -> writeIORef fishing MarinerAngler 
                            ["mariner", "angler"] -> writeIORef fishing MarinerAngler 
                            ["mariner&angler"] -> writeIORef fishing MarinerAngler 
                            _ -> do
                                putStrLn "Wasn't able to read your fishing skill. Available options are 'nofishing', 'fisher', 'angler', 'mariner', 'marinerangler'"
                                putStrLn "Nothing changed.\n"
                        "sashimi" -> case l of
                            ["false"] -> writeIORef sashimi False
                            ["no"]    -> writeIORef sashimi False
                            ["true"]  -> writeIORef sashimi True
                            ["yes"]   -> writeIORef sashimi True
                            _ -> do
                                putStrLn "Wasn't able to change if you want to make every fish below 75g into sashimi before selling. Available options are 'true' and 'false'. (You can also write 'yes' or 'no'.)"
                                putStrLn "Nothing changed.\n"
                        _ -> do
                            putStrLn "Wasn't able to read what variable you are intending to change. Available options are 'place', 'farming', 'fishing' and 'sashimi'."
                            putStrLn "Nothing changed.\n"
                    printcase'
                    main'
                "profit":_ -> do
                    putStrLn $ "You would make a profit of (" ++ show profit ++ ") \8776 " ++ roundFraction 2 profit ++ " gold every day."
                    putStrLn $ "Note, that haskell is using '%' for fractions."
                    putStrLn $ "If you want a reasoning for that number type command 'proof'."
                    main'
                "proof":_  -> do

                    putStrLn $ "You would make a profit of (" ++ show profit ++ ") \8776 " ++ roundFraction 2 profit ++ " gold every day."
                    putStrLn $ "The number can be optained by the following probability tree. Just add the product of probability and selling value of any leaf in the tree together and you will get the number " ++ roundFraction 2 profit ++ "."
                    putStrLn $ "NOTE: Haskell is using '%' to print fractions."
                    if a == NoRecycling
                      then putStrLn "NOTE: You are not selling the trash. Therefore all the recycling items (Stone, Coal, Wood, IronOre, Cloth, Torch) get a selling value of 0." 
                      else return ()
                    putStrLn ""
                    putStrLn $ showTree $ intoProofLines i a s tree
                    main'
                _ -> putStrLn "Unknown command. Type 'help' if you want a list of valid commands." >> main'
    main'        

printcase :: IORef Place -> IORef Farming -> IORef Fishing -> IORef Bool -> IO ()
printcase place farming fishing sashimi = do
    p <- readIORef place
    a <- readIORef farming
    i <- readIORef fishing
    s <- readIORef sashimi
    putStrLn "This is the actual state to look at:"
    case p of
        InOcean -> putStrLn "The crab pot is inside the ocean."
        InFresh -> putStrLn "The crab pot is inside the freshwater."
    case a of
        NoRecycling -> putStrLn "You do not plan to recycle the trash. Therefore your farming profession doesnt matter."
        NoFarming   -> putStrLn "You want to recycle your trash in a recycling machine. Your farming skill doesnt make the cloth more valuable. (e.g. it isnt rancher or artisan)"
        Rancher     -> putStrLn "You want to recycle your trash in a recycling machine and your farming skill is rancher. Therefore cloth is 20% more valueable."
        Artisan     -> putStrLn "You want to recycle your trash in a recycling machine and your farming skill is artisan. Therefore cloth is 40% more valueable."
    case i of
        NoFishing     -> putStrLn "You have the lurelaster profession or no fishing profession."
        Fisher        -> putStrLn "You have the fishing profession but not the angler profession."
        Angler        -> putStrLn "You have the angler profession."
        Mariner       -> putStrLn "You have the mariner profession."
        MarinerAngler -> putStrLn "You have the mariner profession but you will switch to the angler profession when you sell the fish."
    case s of
        False -> putStrLn "You will NOT turn the fish with value less then 75g into sashimi."
        True  -> putStrLn "You will turn the fish with value less then 75g into sashimi."

intoProofLines :: Fishing -> Farming -> Bool -> ProbTree Rational Item -> ProbTree Rational ProofLine
intoProofLines fishing farming sashimi tree = (\(prob, item) -> ProofLine prob item (profit fishing farming sashimi item)  )  <$> probabilityIntoLeafs tree

data ProofLine = ProofLine Rational Item Integer

instance Show ProofLine where
    show (ProofLine probability item value) = showItem item ++ " has a probability of (" ++ show probability ++ ") \8776 " ++ showPercentage probability ++ " and a selling value of " ++ show value ++ ". Product of both: (" 
                                                                                        ++ show prod ++ ") \8776 " ++ roundFraction 2 prod
      where
        prod = probability * (value % 1)
        showItem (Trash (n,name) ) = show n ++ "x" ++ show name
        showItem (Ocean o) = show o
        showItem (Fresh f) = show f

help :: IO ()
help = do
    putStrLn "This program helps you to find out how much gold per day you can make using crab pots in stardew valley."
    putStrLn "Initialy the program assumes:"
    putStrLn "    1.) The crab pot is placed in the ocean."
    putStrLn "    2.) That you dont sell the trash / recycling goods."
    putStrLn "    3.) That you dont have any fishing profession."
    putStrLn "    4.) That you dont turn the fish into sashimi before selling."
    putStrLn "    5.) That you do NOT want to include the price of fishing bait."
    putStrLn "You can change the presets of 1 - 4 by using the 'set' command."
    putStrLn ""
    putStrLn "The following commands are available:  'exit', 'q', 'quit', 'help', 'printcase', 'profit', 'proof', 'set'"
    putStrLn "    exit: Ends this program."    
    putStrLn "    q:    Same as exit."    
    putStrLn "    quit: Same as exit."
    putStrLn "    help: Prints the text you are currently reading."
    putStrLn "    printcase: Shows the assumptions for the calculation. (See 1. - 5.) "
    putStrLn "    profit: Shows you how much profit you would make using a crab pot (given the assumption (1. - 5.) from above)."
    putStrLn "    proof: Gives a reasoning for the profit by printing the entire probability tree. "
    putStrLn "    set [variable] [value]: Changes one of the assumptions. Valid values for variable are 'place', 'farming', 'fishing' and 'sashimi'."
    putStrLn ""
    putStrLn "     > set place ocean           (preset)"
    putStrLn "     > set place freshwater"
    putStrLn ""
    putStrLn "    If you plan to sell the recycling goods, it is important what farming profession you got, because the selling price of cloth depends on that."
    putStrLn "    You can also pick 'nofarming' if you will sell the recycling goods but selling cloth for its standard price of 470g."
    putStrLn "     > set farming norecycling   (preset)"
    putStrLn "     > set farming nofarming"
    putStrLn "     > set farming rancher"
    putStrLn "     > set farming artisan"
    putStrLn ""
    putStrLn "    Select your fishing perfection. 'marinerangler' means, that you pick mariner to collect the fish but switching for one day to angler by the " 
    putStrLn "    dog statue to sell for a better price."
    putStrLn "     > set fishing nofishing     (preset)"
    putStrLn "     > set fishing fisher"
    putStrLn "     > set fishing angler"
    putStrLn "     > set fishing mariner"
    putStrLn "     > set fishing marinerangler"
    putStrLn ""
    putStrLn "    Select if you will turn all the fish with a selling price less then 75g into sashimi before selling. "
    putStrLn "     > set sashimi false         (preset)"
    putStrLn "     > set sashimi true"
