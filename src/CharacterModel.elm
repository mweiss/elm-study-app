module CharacterModel (CharacterModel, Sentence, characterModel, sentenceModel, tokenizePinyin, 
  WorkbookProblem, workbookDecoder, workbookProblemToSentence, grade, inputToPinyin, Grade(..)) where

import Dict exposing (Dict)
import String
import Regex exposing (Regex)
import List
import Char

import Json.Decode exposing ((:=))
import Json.Decode as JSD

type alias Sentence =
  { chinese : String
  , pinyin  : String
  }

type alias CharacterModel answer =
  { chinese : String
  , pinyin : Maybe String
  , answers : List answer
  , selected : Bool
  , currentAnswer : Maybe answer
  }

type alias WorkbookProblem = {
  chinese : String,
  pinyin  : String,
  chapter : Int,
  book    : Int
}

-- GRADE

type Grade = Correct | Incorrect | NotGraded | DoNotGrade

grade : CharacterModel String -> Grade
grade characterModel =
  case characterModel.pinyin of
    Nothing -> DoNotGrade
    Just p  -> case (List.head characterModel.answers) of
      Nothing -> NotGraded
      Just a  -> if String.toLower p == (String.toLower (inputToPinyin a)) 
        then Correct 
        else Incorrect

-- Helper method which takes user input and converts it into pinyin.  For example,
-- this method will convert a string like wo3 to wǒ.  This method only returns valid
-- pinyin if the input is valid pinyin.
inputToPinyin : String -> String
inputToPinyin input = 
  let n = case String.toInt (String.right 1 input) of
        Err _ -> -1
        Ok number -> number
      regexString = pinyinSyllableToRegexString (Regex.replace (Regex.AtMost 1) (Regex.regex "[0-9]$") (\_ -> "") input)
  in  if n < 0 || n > 4
        then input
        else Regex.replace
          (Regex.AtMost 1)
          (Regex.regex "\\[.*\\]")
          (\match -> String.slice (n + 1) (n + 2) match.match)
          regexString

workbookProblemToSentence : WorkbookProblem -> Sentence
workbookProblemToSentence wp = {chinese = wp.chinese, pinyin = wp.pinyin}

workbookDecoder : JSD.Decoder (List WorkbookProblem)
workbookDecoder = 
  JSD.list (JSD.object4 WorkbookProblem
    ("chinese" := JSD.string)
    ("pinyin" := JSD.string)
    ("chapter" := JSD.int)
    ("book" := JSD.int))

-- Helper methods to transform a sentence into a character model

sentenceModel : Sentence -> List (CharacterModel answer)
sentenceModel sentence = List.map2 characterModel (String.split "" sentence.chinese) (tokenizePinyin sentence.pinyin)

characterModel : String -> String -> CharacterModel answer
characterModel chinese pinyin =
  { chinese = chinese
  , pinyin =  if (Regex.contains (Regex.regex "[,.?!]") pinyin) then Nothing else (Just pinyin)
  , answers = []
  , selected = False 
  , currentAnswer = Nothing
  }

tokenizePinyin : String -> List String
tokenizePinyin s = List.filter (\v -> v /= " ") (List.map (\x -> x.match) (Regex.find Regex.All pinyinRegex s))

pinyinRegex : Regex
pinyinRegex = Regex.caseInsensitive (Regex.regex stringRegex)

stringRegex = (String.join "|" (List.append ["[,.?! ]"] (List.foldr foldPinyinSyllables [] pinyinSyllables)))

foldPinyinSyllables : String -> List String -> List String
foldPinyinSyllables pinyin l =  List.append [pinyinSyllableToRegexString pinyin] l

getDefaultToKey : String -> Dict String String -> String
getDefaultToKey key dict = resolveMaybe key (Dict.get key dict)

-- TODO: I actually want this to fail if it breaks.  This is a convenience so I don't
-- have multi-line case statements everywhere doing the same thing
resolveMaybe : v -> Maybe v -> v
resolveMaybe default maybe =
  case maybe of
    Nothing -> default
    Just value -> value

pinyinSyllableToRegexString : String -> String
pinyinSyllableToRegexString pinyin =
  let vowel = String.filter (\c -> Dict.member (String.fromChar c) vowelToTonesMap) pinyin
      vowelRegex = getDefaultToKey vowel pinyinVowelsToRegex
  in  Regex.replace
        (Regex.AtMost 1)
        (Regex.regex vowel)
        (\_ -> vowelRegex)
        pinyin

pinyinVowelsToRegex : Dict String String
pinyinVowelsToRegex = List.foldr foldPinyinVowels Dict.empty pinyinVowels

pinyinToRegex : String -> Int -> String
pinyinToRegex s i =
  let leftStr = String.left i s
      rightStr = String.dropLeft (i + 1) s
      vowel = String.slice i (i + 1) s
      lookupValue = getDefaultToKey vowel vowelToTonesMap
  in  leftStr ++ "[" ++ lookupValue ++ "]" ++ rightStr

foldPinyinVowels : String -> Dict String String -> Dict String String
foldPinyinVowels pinyin dict =
  let pinyinIndex = (resolveMaybe 1 (List.head (String.indexes "*" pinyin))) - 1
      key = String.filter (\c -> c /= '*') pinyin
  in  (Dict.insert key (pinyinToRegex key pinyinIndex) dict)

vowelToTonesMap : Dict String String
vowelToTonesMap = 
  Dict.fromList
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

pinyinVowels : List String
pinyinVowels =
  [ "a*", "o*", "e*", "i*", "er"
  , "a*i", "e*i", "a*o", "o*u"
  , "ia*", "ia*o", "ie*", "iu*", "io*"
  , "u*", "ua*i", "ua*", "ue*", "ui*", "uo*"
  , "üe*", "i*", "ü*"
  ]

pinyinSyllables = List.reverse <| List.sortBy String.length
  [ "a", "o", "e", "er", "ai", "ao", "ou", "an", "en", "ang", "eng"
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