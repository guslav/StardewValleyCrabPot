module Main (main) where

import Data.IORef
--import Data.Ratio ((%))
import Data.Char (toLower)
import Data.List (intercalate)

import System.IO (hSetBuffering, BufferMode(..) , stdout)

import Control.Monad.Writer.Lazy (tell, execWriter)

import System.Directory (createDirectoryIfMissing)

import StardewValley

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    place   <- newIORef InOcean
    farming <- newIORef NoRecycling
    fishing <- newIORef NoFishing
    sashimi <- newIORef False
    let printcase' = printcase place farming fishing sashimi
    putStrLn ""
    putStrLn ""
    putStrLn "Welcome to the stardew valley crab pot calculator. Type 'help' if you want to see a list of the available commands."
    let main' = do
            p <- readIORef place
            a <- readIORef farming
            i <- readIORef fishing
            s <- readIORef sashimi
            putStr "\nStardewValley> "
            command <- words <$> map toLower <$> getLine
            let tree = getProbabilityTree p i :: ProbTree Rational Item
            let avprofit = averageProfit i a s tree
            case command of
                "q":_      -> putStrLn "bye!"
                "quit":_   -> putStrLn "bye!"
                "exit":_   -> putStrLn "bye!"
                "help":_   -> help >> main'
                "printcase":_ -> printcase' >> main'
                "printtable":_ -> printtable >> main'
                "onlinetable":_      -> putStrLn onlineTable >> main'
                "onlinetabletrash":_ -> putStrLn onlineTableTrash >> main'
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
                    putStrLn $ "You would make a profit of (" ++ show avprofit ++ ") \8776 " ++ roundFraction 2 avprofit ++ " gold every day."
                    putStrLn $ "Note, that haskell is using '%' for fractions."
                    putStrLn $ "If you want a reasoning for that number type command 'proof'."
                    main'
                "proof":_  -> putStrLn (showProof p a i s) >> main'
                "writeproofs":_ -> writeProofs >> main'
                _ -> putStrLn "Unknown command. Type 'help' if you want a list of valid commands." >> main'
    main'        

writeProofs :: IO ()
writeProofs = do
    let folder = "calculations"
    createDirectoryIfMissing False folder
    allCases $ \place farming fishing sashimi -> do
            let filename = (++ ".txt") $ intercalate "_" $ [show place, show farming, show fishing, showSashimi sashimi]
            let content = filename ++ "\n" ++ showProof place farming fishing sashimi
            writeFile (folder ++ "/" ++ filename) content
  where
    allCases :: (Enum a, Bounded a, Enum b, Bounded b, Enum c, Bounded c, Enum d, Bounded d) => (a -> b -> c -> d -> IO ()) -> IO ()
    allCases f = sequence_ $ f <$> everything <*> everything <*> everything <*> everything
    showSashimi :: Bool -> String
    showSashimi s
      | s = "Sashimi"
      | otherwise = "NoSashimi"

showProof :: Place -> Farming -> Fishing -> Bool -> String
showProof place farming fishing sashimi = execWriter $ do
    let tellLn = tell . (++"\n")
    let tree = getProbabilityTree place fishing :: ProbTree MyPercentage Item
    let avprofit = fromRational $ toRational $ averageProfit fishing farming sashimi tree :: MyRational
    tellLn $ "\n\nYou would make a profit of " ++ show avprofit ++ " gold every day."
    tellLn $ "The number can be obtained by the following probability tree. Just add the product of probability and selling value of any leaf in the tree together and you will get the number " ++ roundFraction 2 avprofit ++ "."
    if farming == NoRecycling
      then tellLn "NOTE: You are not selling the trash. Therefore all the recycling items (Stone, Coal, Wood, IronOre, Cloth, Torch) get a selling value of 0." 
      else return ()
    tellLn ""
    tellLn $ showTree $ intoProofLines fishing farming sashimi tree

printtable :: IO ()
printtable = do
  putStrLn $ trimmer ["Place", "FishingSkill", "FarmingSkill", "Sashimi", "profit", "exact average profit (fraction)"]
  allCases summulateCertain
  where
    allCases :: (Enum a, Bounded a, Enum b, Bounded b, Enum c, Bounded c, Enum d, Bounded d) => (a -> b -> c -> d -> IO ()) -> IO ()
    allCases f = sequence_ $ f <$> everything <*> everything <*> everything <*> everything
    trimmer :: [String] -> String
    trimmer [a,b,c,d,e,f] = intercalate " " [trim 7 a, trim 12 b, trim 12 c, trim 8 d, trim 8 e, f]
    trimmer _ = error "something didnt work."
    trim :: Int -> String -> String
    trim n str = take n $ str ++  repeat ' '
    summulateCertain place fishing sashimi farming  = do
        let tree = getProbabilityTree place fishing :: ProbTree Rational Item
        let avprofit = averageProfit fishing farming sashimi tree :: Rational
        putStrLn $ trimmer [show place, show fishing, show farming, show sashimi, roundFraction 2 avprofit, show avprofit]

onlineTable :: String
onlineTable = (top ++) $ (++ "|}") $ do
    place <- everything
    sashimi <- everything
    left place sashimi ++ right place sashimi
  where
    top = "{| class=\"wikitable\"\n|\n! [[Luremaster]] or no Fishing Profession \n! [[Fisher]]\n! [[Fisher|Angler]]\n! [[Mariner]]\n! [[Fisher|Angler]] & [[Mariner]]\n"
    left :: Place -> Bool -> String
    left place sashimi = "|- \n! " ++ showPlace place ++ s ++ "\n"
        where s = if sashimi then " + [[Sashimi]]" else ""
    right :: Place -> Bool -> String
    right place sashimi = "| " ++ cell NoFishing ++ "\n| " ++ cell Fisher ++ "\n| " ++ cell Angler ++ "\n| " ++ cell Mariner ++ "\n| " ++ cell MarinerAngler ++ "\n"
      where
        cell :: Fishing -> String
        cell fishing = roundFraction 2 $ (averageProfit fishing NoRecycling sashimi $ getProbabilityTree place fishing :: Rational)

onlineTableTrash :: String
onlineTableTrash = (top ++) $ (++ "|}") $ do
    place <- everything
    left place ++ right place
  where
    top = "{| class=\"wikitable\"\n|\n! [[Agriculturist]] or no Farming Profession  \n! [[Rancher]]\n! [[Artisan]]\n"
    left :: Place -> String
    left place = "|- \n! " ++ showPlace place ++ "\n"
    right :: Place -> String
    right place = "| " ++ cell NoFarming ++ "\n| " ++ cell Rancher ++ "\n| " ++ cell Artisan ++ "\n"
      where
        cell :: Farming -> String
        cell farming =  roundFraction 2 $ prof farming - prof NoRecycling
        prof :: Farming -> Rational
        prof farming = (averageProfit NoFishing farming False $ getProbabilityTree place NoFishing :: Rational)

showPlace :: Place -> String
showPlace InOcean = "Ocean"
showPlace InFresh = "Freshwater"

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

intoProofLines :: (Fractional d) => Fishing -> Farming -> Bool -> ProbTree d Item -> ProbTree d (ProofLine d)
intoProofLines fishing farming sashimi tree = (\(prob, item) -> ProofLine prob item (profit fishing farming sashimi item)  )  <$> probabilityIntoLeafs tree

data ProofLine d = ProofLine d Item Integer

instance (RealFrac d, Show d) => Show (ProofLine d) where
    show (ProofLine probability item value) = showItem item ++ " has a probability of " ++ show probability ++ " and a selling value of " ++ show value ++ ". Product of both: " ++ show prod
      where
        prod = fromRational $ toRational $ probability * fromInteger value :: MyRational
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
    putStrLn "The following commands are available:  'exit', 'q', 'quit', 'help', 'printcase', 'profit', 'proof', 'printtable', 'onlinetable', 'onlinetabletrash', 'set'"
    putStrLn "    exit: Ends this program."    
    putStrLn "    q:    Same as exit."    
    putStrLn "    quit: Same as exit."
    putStrLn "    help: Prints the text you are currently reading."
    putStrLn "    printcase: Shows the assumptions for the calculation. (See 1. - 5.) "
    putStrLn "    profit: Shows you how much profit you would make using a crab pot. (depends on the assumptions)"
    putStrLn "    proof: Gives a reasoning for the profit by printing the entire probability tree. "
    putStrLn "    printtable: prints a table with the profits under each possible assumption set (see 1. - 5. above) "
    putStrLn "    onlinetable: prints a String, that makes a table for stardewvalleywiki.com. See 'https://stardewvalleywiki.com/Talk:Crab_Pot#Calculating_the_gold_per_day' to see an example."
    putStrLn "    onlinetabletrash: prints a String, that makes a table for stardewvalleywiki.com. See 'https://stardewvalleywiki.com/Talk:Crab_Pot#Calculating_the_gold_per_day' to see an example."
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
    putStrLn "    dog statue to sell for a better price. The cost of 10.000g is excluded. collect the fish for a while in a box."
    putStrLn "     > set fishing nofishing     (preset)"
    putStrLn "     > set fishing fisher"
    putStrLn "     > set fishing angler"
    putStrLn "     > set fishing mariner"
    putStrLn "     > set fishing marinerangler"
    putStrLn ""
    putStrLn "    Select if you will turn all the fish with a selling price less then 75g into sashimi before selling. "
    putStrLn "     > set sashimi false         (preset)"
    putStrLn "     > set sashimi true"
