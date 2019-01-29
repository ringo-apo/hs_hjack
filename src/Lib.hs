module Lib
    ( play
    ) where

import           Control.Monad         (liftM2)
import           Data.Char             (toLower)
import           Data.List             (intercalate, sort)
import           System.Random.Shuffle (shuffleM)

data Card = CardA | Card2 | Card3 | Card4 | Card5
            | Card6 | Card7 | Card8 | Card9 | Card10
            | CardJ | CardQ | CardK
            deriving (Eq, Ord, Enum)

instance Show Card where
    show CardA = "A"
    show CardJ = "J"
    show CardQ = "Q"
    show CardK = "K"
    show card  = show $ fromEnum card + 1

cardValue :: Card -> [Int]
cardValue CardA = [1, 11]
cardValue CardJ = [10]
cardValue CardQ = [10]
cardValue CardK = [10]
cardValue card  = [fromEnum card + 1]

type Hands = [Card]
type Deck = [Card]

allCards :: Deck
allCards = concatMap (replicate 4) [CardA ..]

showHands :: Hands -> String
showHands hands = intercalate ", " $ map show (sort hands)

countHands :: Hands -> Int
countHands hands = snd $ maximum $ map (\c -> (powerHands c, c)) $
        foldl (liftM2 (+)) [0] $ map cardValue hands


powerHands :: Int -> Int
powerHands handsCount
    | handsCount > 21 = 0
    | handsCount <  1 = 0
    | otherwise       = handsCount

data Field = Field
    { playerHands :: Hands
    , dealerHands :: Hands
    , fieldDeck   :: Deck
    }

initField :: IO Field
initField = deal <$> shuffleM allCards
        where
            deal (c1:c2:c3:c4:cards) = Field
                { playerHands = [c1, c3]
                , dealerHands = [c2, c4]
                , fieldDeck = cards
                }

play :: IO ()
play = initField
        >>= printInitialField
        >>= playerTurn
        >>= dealerTurn
        >>= gameResult
    where
        printInitialField field = do
            putStrLn "ディーラーの初期手札（2枚のうち片方）："
            putStrLn $ ' ' : show (head (dealerHands field))
            putStrLn ""
            return $ Right field

draw :: Deck -> (Maybe Card, Deck)
draw []       = (Nothing, [])
draw (c:deck) = (Just c, deck)

isBust :: Int -> Bool
isBust = (> 21)

playerTurn :: Either Field Field -> IO (Either Field Field)
playerTurn = either whenError whenNormal
    where
        whenError = return . Left
        whenNormal field = do
            let hands = playerHands field
            let count = countHands hands
            putStrLn $ "現在のあなたの手札：" ++ showHands hands
            if isBust count then do
                putStrLn "バーストしました。"
                return $ Left field
            else do
                let (maybeNextCard, nextDeck) = (draw . fieldDeck) field
                maybe whenDeckEmpty (whenDeckRemain nextDeck) maybeNextCard
                where
                    whenDeckEmpty = do
                        putStrLn "もう山札がありません。"
                        return $ Left field
                    whenDeckRemain nextDeck nextCard = do
                        putStrLn "どうしますか？ Hit/Stand"
                        input <- map toLower <$> getLine
                        case input of
                            "hit" -> let hands = playerHands field
                                        in playerTurn $ Right field
                                            { playerHands = nextCard : hands
                                            , fieldDeck = nextDeck
                                            }
                            _     -> return $ Right field


dealerTurn :: Either Field Field -> IO (Either Field Field)
dealerTurn = either whenError whenNormal
    where
        whenError = return . Left
        whenNormal field = do
            let hands = dealerHands field
            let count = countHands hands
            if isBust count then do
                putStrLn "ディーラーがバーストしました。"
                return $ Left field
            else do
                let (maybeNextCard, nextDeck) = draw $ fieldDeck field
                maybe whenDeckEmpty (whenDeckRemain nextDeck) maybeNextCard
                where
                    whenDeckEmpty = return $ Left field
                    whenDeckRemain nextDeck nextCard = do
                        let hands = dealerHands field
                        let count = countHands hands
                        if count < 17 then
                            dealerTurn $ Right
                                field{dealerHands = nextCard : hands, fieldDeck = nextDeck}
                        else return $ Right field


gameResult :: Either Field Field -> IO ()
gameResult resultField = do
    let field = either id id resultField
    let playerCount = countHands $ playerHands field
    let dealerCount = countHands $ dealerHands field
    putStrLn ""
    putStrLn "結果："
    putStrLn $ "あなたの手札：" ++ showHands (playerHands field)
    putStrLn $ "ポイント合計：" ++ show playerCount
    putStrLn ""
    putStrLn $ "ディーラーの手札：" ++ showHands (dealerHands field)
    putStrLn $ "ポイント合計：" ++ show dealerCount
    putStrLn ""
    let playerPower = powerHands playerCount
    let dealerPower = powerHands dealerCount
    putStrLn $ judge playerPower dealerPower
    where
        judge playerPower dealerPower
            | playerPower > dealerPower = "あなたの勝ち"
            | playerPower < dealerPower = "ディーラーの勝ち"
            | otherwise                 = "引き分け"
