module CharacterModel (CharacterModel, Sentence, characterModel, tokenizePinyin) where

import Dict exposing (Dict)
import String

type alias Sentence =
  {
  , chinese : String
  , pinyin  : String
  }

type alias CharacterModel answer =
  {
  , chinese : String
  , pinyin : Maybe String
  , word : String
  , answers : List answer
  , selected : Bool
  }


pinyinRegex : Regex
pinyinRegex = 
  let 
  in 

pinyinSyllableToRegex pinyin =
  let vowel = String.filter (\c -> Dict.member (fromChar c) vowelToTonesMap) pinyin


-- Helper methods to transform a sentence into a character model
characterModel : Sentence -> List CharacterModel
characterModel sentence = [] -- TODO: implement

tokenizePinyin : String -> List String
tokenizePinyin s = 

vowelToTonesMap : Dict String String
vowelToTonesMap = 
  Dict.fromMap
    [ ("a", "aāáǎà")
    , ("e", "eēéěè")
    , ("i", "iīíǐì")
    , ("o", "oōóǒò")
    , ("u", "uūúǔù")
    , ("ü", "üǖǘǚǜ")
    , ("A", "AĀÁǍÀ")
    , ("E", "EĒÉĚÈ")
    , ("I", "IĪÍǏÌ")
    , ("O", "OŌÓǑÒ")
    , ("U", "UŪÚǓÙ")
    , ("Ü", "ÜǕǗǙǛ")
    ]

pinyinVowelsToRegex : Dict String String
pinyinVowelsToRegex = List.foldr foldPinyinVowels Dict.empty 

capitalize : String -> String
capitalize s = String.append (String.capitalize (String.left 1 s)) (String.dropLeft 1 s)

pinyinToRegex : String -> Index -> String
pinyinToRegex s i =
  let leftStr = String.left i s
      rightStr = String.dropLeft (i + 1) s
      vowel = String.slice i (i + 1) s,
      lookup = Dict.get vowel vowelToTonesMap
      lookupValue = case lookup of
        Nothing -> "" -- TODO: Cannot happen. What's the fail value?
        Just a -> a
  in  leftStr ++ "[" ++ a ++ "]" ++ rightStr

foldPinyinVowels : String -> Dict -> Dict
foldPinyinVowels pinyin dict =
  let pinyinIndex = (String.indexes pinyin "*") - 1
      pinyinWithoutStar = String.filter (\c -> c != '*') pinyin
      capitalizedPinyinWithoutStar = capitalize pinyinWithoutStar
      pinyinToRegex = 

pinyinVowels : List String
pinyinVowels = 
  [ "a*", "o*", "e*", "i*", "er"
  , "a*i", "e*i", "a*o", "o*u"
  , "ia*", "ia*o", "ie*", "iu*", "io*"
  , "u*", "ua*i", "ua*", "ue*", "ui*", "uo*"
  , "üe*", "i*", "ü*"
  ]

pinyinSounds = 
  [ "o", "e", "er", "ai", "ao", "ou", "an", "en", "ang", "eng"
  , "yi", "ya", "yao", "ye", "you", "yan", "yin", "yang", "ying", "yong"
  , "wu", "wa", "wo","wai", "wei", "wan", "wen", "wang", "weng"
  , "yu", "yue", "yuan", "yun"

  , "ba", "bo", "bai", "bei", "bao", "ban", "ben", "bang", "beng", "bi"
  , "biao", "bie", "bian", "bin", "bing", "bu"

  , "pa", "po", "pai", "pei", "pao", "pou", "pan", "pen", "pang", "peng", "pi"
  , "piao", "pie", "pian", "pin", "ping", "pu"

  , "ma", "mo", "me", "mai", "mei", "mao", "mou", "man", "men", "mang", "meng", "mi", "miao"
  , "mie", "miu", "mian", "min", "ming", "mu"

  , "fa", "fo", "fei", "fou", "fan", "fen", "fang", "feng", "fu"

  , "da", "de", "dai", "dei", "dao", "dou", "dan", "den", "dang", "deng"
  , "dong", "di", "diao", "die", "diu", "dian", "ding", "du", "duo", "dui"
  , "duan", "dun"

  , "ta", "te", "tai", "tei", "tao", "tou", "tan", "tang", "teng", "tong", "ti", "tiao", "tie"
  , "tian", "ting", "tu", "tuo", "tui", "tuan", "tun"

  , "na", "ne", "nai", "nei", "nao", "nou", "nan", "nen", "nang", "neng", "nong", "ni", "niao"
  , "nie", "niu", "nian", "nin", "niang", "ning", "nu", "nuo", "nuan", "nü", "nüe"

  , "la", "le", "lai", "lei", "lao", "lou", "lan", "lang", "leng", "long", "li", "lia", "liao", "lie"
  , "liu", "lian", "lin", "liang", "ling", "lu", "luo", "luan", "lun", "lü", "lüe"

  , "ga", "ge", "gai", "gei", "gao", "gou", "gan", "gen", "gang", "geng", "gong", "gu", "gua", "guo"
  , "guai", "gui", "guan", "gun", "guang"

  , "ka", "ke", "kai", "kei", "kao", "kou", "kan", "ken", "kang", "keng", "kong", "ku", "kua", "kuo"
  , "kuai", "kui", "kuan", "kun", "kuang"

  , "ha", "he", "hai", "hei", "hao", "hou", "han", "hen", "hang", "heng", "hong", "hu", "hua", "huo"
  , "huai", "hui", "huan", "hun", "huang"

  , "za", "ze", "zi", "zai", "zei", "zao", "zou", "zan", "zen", "zang", "zeng", "zong", "zu", "zuo"
  , "zui", "zuan", "zun"

  , "ca", "ce", "ci", "cai", "cao", "cou", "can", "cen", "cang", "ceng", "cong", "cu", "cuo", "cui"
  , "cuan", "cun"

  , "sa", "se", "si", "sai", "sao", "sou", "san", "sen", "sang", "seng", "song", "su", "suo", "sui"
  , "suan", "sun"

  , "zha", "zhe", "zhi", "zhai", "zhei", "zhao", "zhou", "zhan", "zhen", "zhang", "zheng", "zhong"
  , "zhu", "zhua", "zhuo", "zhuai", "zhui", "zhuan", "zhun", "zhuang"

  , "cha", "che", "chi", "chai", "chao", "chou", "chan", "chen", "chang", "cheng", "chong", "chu"
  , "chua", "chuo", "chuai", "chui", "chuan", "chun", "chuang"

  , "sha", "she", "shi", "shai", "shei", "shao", "shou", "shan", "shen", "shang", "sheng", "shu"
  , "shua", "shuo", "shuai", "shui", "shuan", "shun", "shuang"

  , "re", "ri", "rao", "rou", "ran", "ren", "rang", "reng", "rong", "ru", "rua", "ruo", "rui"
  , "ruan", "run"

  , "ji", "jia", "jiao", "jie", "jiu", "jian", "jin", "jiang", "jing", "jiong", "ju", "jue", "juan", "jun"

  , "qi", "qia", "qiao", "qie", "qiu", "qian", "qin", "qiang", "qing", "qiong", "qu", "que", "quan", "qun"

  , "xi", "xia", "xiao", "xie", "xiu", "xian", "xin", "xiang", "xing", "xiong", "xu", "xue", "xuan", "xun"
  ]