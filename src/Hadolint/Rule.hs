module Hadolint.Rule where

import Control.DeepSeq (NFData)
import Data.Default
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Language.Docker.Syntax
import Prettyprinter (Pretty, pretty)
import qualified Control.Foldl as Foldl
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.YAML as Yaml

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- 定义Dockerfile lint规则的严重程度
data DLSeverity
  = DLErrorC    -- 错误
  | DLWarningC  -- 警告
  | DLInfoC     -- 信息
  | DLStyleC    -- 风格问题
  | DLIgnoreC   -- 忽略
  deriving (Eq, Ord, Show, Generic, NFData)

instance Yaml.FromYAML DLSeverity where
  parseYAML = withSeverity pure

withSeverity ::
  (DLSeverity -> Yaml.Parser a) ->
  Yaml.Node Yaml.Pos ->
  Yaml.Parser a
withSeverity f v@(Yaml.Scalar _ (Yaml.SStr b)) =
  case readEitherSeverity b of
    Right s -> f s
    Left _ -> Yaml.typeMismatch "severity" v
withSeverity _ v = Yaml.typeMismatch "severity" v

readEitherSeverity :: Text -> Either String DLSeverity
readEitherSeverity "error" = Right DLErrorC
readEitherSeverity "warning" = Right DLWarningC
readEitherSeverity "info" = Right DLInfoC
readEitherSeverity "style" = Right DLStyleC
readEitherSeverity "ignore" = Right DLIgnoreC
readEitherSeverity "none" = Right DLIgnoreC
readEitherSeverity t = Left ("Invalid severity: " ++ unpack t)

readMaybeSeverity :: Text -> Maybe DLSeverity
readMaybeSeverity "error" = Just DLErrorC
readMaybeSeverity "warning" = Just DLWarningC
readMaybeSeverity "info" = Just DLInfoC
readMaybeSeverity "style" = Just DLStyleC
readMaybeSeverity "ignore" = Just DLIgnoreC
readMaybeSeverity "none" = Just DLIgnoreC
readMaybeSeverity _ = Nothing

instance Semigroup DLSeverity where _ <> s2 = s2

instance Monoid DLSeverity where mempty = DLIgnoreC

instance Default DLSeverity where
  def = DLInfoC

instance Pretty DLSeverity where
  pretty DLErrorC = "error"
  pretty DLWarningC = "warning"
  pretty DLInfoC = "info"
  pretty DLStyleC = "style"
  pretty DLIgnoreC = "ignore"


-- 规则代码类型
newtype RuleCode = RuleCode {unRuleCode :: Text}
  deriving (Eq, Ord)

instance Show RuleCode where
  show rc = show (unRuleCode rc)

instance IsString RuleCode where
  fromString = RuleCode . Text.pack

instance Pretty RuleCode where
  pretty rc = pretty $ show rc


-- 检查失败记录
data CheckFailure = CheckFailure
  { code :: RuleCode,     -- 规则代码
    severity :: DLSeverity, -- 严重程度
    message :: Text.Text,   -- 错误信息
    line :: Linenumber     -- 行号
  }
  deriving (Show, Eq)

instance Ord CheckFailure where
  a `compare` b = line a `compare` line b


type Failures = Seq.Seq CheckFailure


-- 状态类型，包含失败记录和自定义状态
data State a = State
  { failures :: Failures,  -- 失败记录序列
    state :: a             -- 自定义状态
  }
  deriving (Show)


type LabelName = Text.Text

-- 标签类型定义
data LabelType
  = Email    -- 邮箱
  | GitHash  -- Git哈希
  | RawText  -- 纯文本
  | Rfc3339  -- RFC3339时间格式
  | SemVer   -- 语义化版本
  | Spdx     -- SPDX许可证标识
  | Url      -- URL
  deriving (Eq, Show)

readEitherLabelType :: Text -> Either Text LabelType
readEitherLabelType "email" = Right Email
readEitherLabelType "hash" = Right GitHash
readEitherLabelType "text" = Right RawText
readEitherLabelType "rfc3339" = Right Rfc3339
readEitherLabelType "semver" = Right SemVer
readEitherLabelType "spdx" = Right Spdx
readEitherLabelType "url" = Right Url
readEitherLabelType "" = Right RawText
readEitherLabelType t = Left ("invalid label type: " <> t)

instance Yaml.FromYAML LabelType where
  parseYAML = withLabelType pure

withLabelType :: (LabelType -> Yaml.Parser a) -> Yaml.Node Yaml.Pos -> Yaml.Parser a
withLabelType f v@(Yaml.Scalar _ (Yaml.SStr b)) =
    case readEitherLabelType b of
      Right lt -> f lt
      Left _ -> Yaml.typeMismatch "labeltype" v
withLabelType _ v = Yaml.typeMismatch "labeltype" v

instance Pretty LabelType where
  pretty RawText = "text"
  pretty Url = "url"
  pretty Spdx = "spdx"
  pretty GitHash = "hash"
  pretty Rfc3339 = "rfc3339"
  pretty SemVer = "semver"
  pretty Email = "email"

type LabelSchema = Map.Map LabelName LabelType


withLineNumber ::
  (Linenumber -> t1 -> Instruction args -> t2) ->
  t1 ->
  InstructionPos args ->
  t2
withLineNumber f state InstructionPos {instruction, lineNumber} =
  f lineNumber state instruction

addFail :: CheckFailure -> State a -> State a
addFail failure state@(State fails _) =
  state
    { failures =
        fails
          Seq.|> failure
    }

emptyState :: a -> State a
emptyState = State Seq.empty

simpleState :: State ()
simpleState = State Seq.empty ()

modify :: (a -> a) -> State a -> State a
modify f s@(State _ st) = s {state = f st}

replaceWith :: a -> State a -> State a
replaceWith newState s = s {state = newState}

type Rule args = Foldl.Fold (InstructionPos args) Failures

-- 简单规则定义
simpleRule ::
  RuleCode ->          -- 规则代码
  DLSeverity ->        -- 严重程度
  Text.Text ->         -- 错误信息
  (Instruction args -> Bool) ->  -- 检查函数
  Rule args
simpleRule code severity message checker = customRule step simpleState
  where
    step line s instr
      | checker instr = s
      | otherwise = s |> addFail (CheckFailure code severity message line)

-- 自定义规则，可以维护状态
customRule ::
  (Linenumber -> State a -> Instruction args -> State a) ->  -- 处理函数
  State a ->  -- 初始状态
  Rule args
customRule step initial = veryCustomRule step initial failures

-- | Similarly to 'customRule', it returns a State a for each step, but it has the ability to run a
-- done callback as the last step of the rule. The done callback can be used to transform the state
-- and mark failures for any arbitrary line in the input. This helper is meant for rules that need
-- to do lookahead. Instead of looking ahead, the state should store the facts and make a decision about
-- them once the input is finished.
veryCustomRule ::
  -- | step calculation for the rule. Called for each instruction in the docker file
  -- it must return the state after being modified by the rule
  (Linenumber -> State a -> Instruction args -> State a) ->
  -- | initial state
  State a ->
  -- | done callaback. It is passed the final accumulated state and it should return all failures
  -- found by the rule
  (State a -> Failures) ->
  Rule args
veryCustomRule step = Foldl.Fold (withLineNumber step)

foldArguments :: (a -> b) -> Arguments a -> b
foldArguments applyRule args =
  case args of
    ArgumentsText as -> applyRule as
    ArgumentsList as -> applyRule as

-- | Returns the result of running the check function on the image alias
--   name, if the passed instruction is a FROM instruction with a stage alias.
--   Otherwise, returns True.
aliasMustBe :: (Text.Text -> Bool) -> Instruction a -> Bool
aliasMustBe predicate fromInstr =
  case fromInstr of
    From BaseImage {alias = Just (ImageAlias as)} -> predicate as
    _ -> True

archiveFileFormatExtensions :: [Text.Text]
archiveFileFormatExtensions =
  [ ".tar",
    ".Z",
    ".bz2",
    ".gz",
    ".lz",
    ".lzma",
    ".tZ",
    ".tb2",
    ".tbz",
    ".tbz2",
    ".tgz",
    ".tlz",
    ".tpz",
    ".txz",
    ".xz"
  ]

dropQuotes :: Text -> Text
dropQuotes = Text.dropAround quotes
  where
    quotes '\"' = True
    quotes '\'' = True
    quotes _ = False


-- | Unwraps ONBUILD instructions and applies the rule to the content
--
onbuild :: Rule args -> Rule args
onbuild rule =
  Foldl.prefilter isOnbuild (Foldl.premap unwrapOnbuild rule)
  where
    isOnbuild InstructionPos {instruction = OnBuild {}} = True
    isOnbuild _ = False

    unwrapOnbuild inst@InstructionPos {instruction = OnBuild i} = inst {instruction = i}
    unwrapOnbuild inst = inst
