{-# LANGUAGE QuasiQuotes #-}

module Test.Integration.Framework.TestData
    ( -- * Mnemonics
      chineseMnemonics9
    , chineseMnemonics18
    , frenchMnemonics12
    , frenchMnemonics21
    , invalidMnemonics12
    , invalidMnemonics15
    , japaneseMnemonics12
    , japaneseMnemonics15
    , mnemonics3
    , mnemonics6
    , mnemonics9
    , mnemonics12
    , mnemonics15
    , mnemonics18
    , mnemonics21
    , mnemonics24
    , specMnemonicSentence
    , specMnemonicSecondFactor

    -- * Wallets
    , arabicWalletName
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , russianWalletName
    , wildcardsWalletName

    -- * Helpers
    , addressPoolGapMax
    , addressPoolGapMin
    , passphraseMaxLength
    , passphraseMinLength
    , payloadWith
    , simplePayload
    , updateNamePayload
    , updatePassPayload
    ) where

import Prelude

import Data.Text
    ( Text )
import Test.Integration.Framework.DSL
    ( Payload (..), json )

falseWalletIds :: [(String, String)]
falseWalletIds =
        [ ("40 chars hex", replicate 40 '1')
        , ("40 chars non-hex", replicate 40 'ś')
        , ("39 chars hex", replicate 39 '1')
        , ("41 chars hex", replicate 41 '1')
        ]

mnemonics3 :: [Text]
mnemonics3 = ["diamond", "flee", "window"]

mnemonics6 :: [Text]
mnemonics6 = ["tornado", "canvas", "peasant", "spike", "enrich", "dilemma"]

mnemonics9 :: [Text]
mnemonics9 = ["subway", "tourist", "abstract", "roast", "border", "curious",
    "exercise", "work", "narrow"]

mnemonics12 :: [Text]
mnemonics12 = ["agent", "siren", "roof", "water", "giant", "pepper",
    "obtain", "oxygen", "treat", "vessel", "hip", "garlic"]

mnemonics15 :: [Text]
mnemonics15 = ["network", "empty", "cause", "mean", "expire", "private",
    "finger", "accident", "session", "problem", "absurd", "banner", "stage",
    "void", "what"]

mnemonics18 :: [Text]
mnemonics18 = ["whisper", "control", "diary", "solid", "cattle", "salmon",
    "whale", "slender", "spread", "ice", "shock", "solve", "panel",
    "caution", "upon", "scatter", "broken", "tonight"]

mnemonics21 :: [Text]
mnemonics21 = ["click", "puzzle", "athlete", "morning", "fold", "retreat",
    "across", "timber", "essay", "drill", "finger", "erase", "galaxy",
    "spoon", "swift", "eye", "awesome", "shrimp", "depend", "zebra", "token"]

mnemonics24 :: [Text]
mnemonics24 = ["decade", "distance", "denial", "jelly", "wash", "sword",
    "olive", "perfect", "jewel", "renew", "wrestle", "cupboard", "record",
    "scale", "pattern", "invite", "other", "fruit", "gloom", "west", "oak",
    "deal", "seek", "hand"]

invalidMnemonics12 :: [Text]
invalidMnemonics12 = ["word","word","word","word","word","word","word",
        "word","word","word","word","hill"]

invalidMnemonics15 :: [Text]
invalidMnemonics15 = ["word","word","word","word","word","word","word",
    "word","word","word","word","word","word","word","word"]

specMnemonicSentence :: [Text]
specMnemonicSentence = ["squirrel", "material", "silly", "twice", "direct",
    "slush", "pistol", "razor", "become", "junk", "kingdom", "flee",
    "squirrel", "silly", "twice"]

specMnemonicSecondFactor :: [Text]
specMnemonicSecondFactor = ["squirrel", "material", "silly", "twice",
    "direct", "slush", "pistol", "razor", "become"]

japaneseMnemonics12 :: [Text]
japaneseMnemonics12 = ["そうだん",　"ひよう",　"にもつ",　"やさしい",　"きふく",　
    "ねつい",　"だったい",　"けんてい",　"けいろ",　"ざつがく",　"ほうもん",　"すこし"]

japaneseMnemonics15 :: [Text]
japaneseMnemonics15 = ["うめる", "せんく", "えんぎ", "はんぺん", "おくりがな",
    "さんち", "きなが", "といれ", "からい", "らくだ", "うえる", "ふめん", "せびろ",
    "られつ", "なにわ"]

chineseMnemonics9 :: [Text]
chineseMnemonics9 = ["钢", "看", "磁", "塑", "凤", "魏", "世", "腐", "恶" ]

chineseMnemonics18 :: [Text]
chineseMnemonics18 = ["盗", "精", "序", "郎", "赋", "姿", "委", "善", "酵",
    "祥", "赛", "矩", "蜡", "注", "韦", "效", "义", "冻"]

frenchMnemonics12 :: [Text]
frenchMnemonics12 = ["palmarès", "supplier", "visuel", "gardien", "adorer",
    "cordage", "notifier", "réglage", "employer", "abandon", "scénario",
    "proverbe"]

frenchMnemonics21 :: [Text]
frenchMnemonics21 = ["pliage", "exhorter", "brasier", "chausson", "bloquer",
    "besace", "sorcier", "absurde", "neutron", "forgeron", "geyser",
    "moulin", "cynique", "cloche", "baril", "infliger", "rompre", "typique",
    "renifler", "creuser", "matière"]

russianWalletName :: Text
russianWalletName = "АаБбВвГгДдЕеЁёЖжЗз ИиЙйКкЛлМмНнО оПпРрСсТтУуФф ХхЦцЧчШшЩщЪъ ЫыЬьЭэЮюЯяІ ѢѲѴѵѳѣі"

polishWalletName :: Text
polishWalletName = "aąbcćdeęfghijklłmnoóprsś\r\ntuvwyzżźAĄBCĆDEĘFGHIJKLŁMNOP\rRSŚTUVWYZŻŹ"

kanjiWalletName :: Text
kanjiWalletName = "亜哀挨愛曖悪握圧扱宛嵐安案暗以衣位囲医依委威為畏胃尉異移萎偉椅彙意違維慰\
\遺緯域育一壱逸茨芋引印因咽姻員院淫陰飲隠韻右宇羽雨唄鬱畝浦運雲永泳英映栄\n営詠影鋭衛易疫益液駅悦越謁\
\閲円延沿炎怨宴媛援園煙猿遠鉛塩演縁艶汚王凹\r\n央応往押旺欧殴桜翁奥横岡屋億憶臆虞乙俺卸音恩温穏下化火加\
\可仮何花佳価果河苛科架夏家荷華菓貨渦過嫁暇禍靴寡歌箇稼課蚊牙瓦我画芽賀雅餓介回灰会快戒改怪拐悔海界\
\皆械絵開階塊楷解潰壊懐諧貝外劾害崖涯街慨蓋該概骸垣柿各角拡革格核殻郭覚較隔閣確獲嚇穫学岳楽額顎掛潟\
\括活喝渇割葛滑褐轄且株釜鎌刈干刊甘汗缶\r"

arabicWalletName :: Text
arabicWalletName = "ثم نفس سقطت وبالتحديد،, جزيرتي باستخدام أن دنو. إذ هنا؟ الستار وتنصيب كان. أهّل ايطاليا، بريطانيا-فرنسا قد أخذ. سليمان، إتفاقية بين ما, يذكر الحدود أي بعد, معاملة بولندا، الإطلاق عل إيو."

wildcardsWalletName :: Text
wildcardsWalletName = "`~`!@#$%^&*()_+-=<>,./?;':\"\"'{}[]\\|❤️ 💔 💌 💕 💞 \
\💓 💗 💖 💘 💝 💟 💜 💛 💚 💙0️⃣ 1️⃣ 2️⃣ 3️⃣ 4️⃣ 5️⃣ 6️⃣ 7️⃣ 8️⃣ 9️⃣ 🔟🇺🇸🇷🇺🇸 🇦🇫🇦🇲🇸"

passphraseMinLength :: Int
passphraseMinLength = 10

passphraseMaxLength :: Int
passphraseMaxLength = 255

addressPoolGapMin :: Int
addressPoolGapMin = 10

addressPoolGapMax :: Int
addressPoolGapMax = 100

payloadWith :: Text -> [Text] -> Payload
payloadWith name mnemonics = Json [json| {
     "name": #{name},
     "mnemonic_sentence": #{mnemonics},
     "passphrase": "Secure passphrase"
     } |]

simplePayload :: Payload
simplePayload = Json [json| {
    "name": "Secure Wallet",
    "mnemonic_sentence": #{mnemonics21},
    "passphrase": "Secure passphrase"
    } |]

updateNamePayload :: Text -> Payload
updateNamePayload name = Json [json| {
     "name": #{name}
     } |]

updatePassPayload :: Text -> Text -> Payload
updatePassPayload oldPass newPass = Json [json| {
    "old_passphrase": #{oldPass},
    "new_passphrase": #{newPass}
      } |]