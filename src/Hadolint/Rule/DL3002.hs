module Hadolint.Rule.DL3002 (rule) where

import qualified Data.IntMap.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Hadolint.Rule
import Language.Docker.Syntax (Instruction (..), Linenumber)

type StageLine = Linenumber

type UserLine = Linenumber

data Acc
  = Acc StageLine (Map.IntMap UserLine)
  | Empty
  deriving (Show)

rule :: Rule args
rule = veryCustomRule check (emptyState Empty) markFailures
  where
    code = "DL3002"  -- 规则代码
    severity = DLWarningC  -- 警告级别
    message = "Last USER should not be root"  -- 错误提示信息

    -- 检查函数，处理不同指令
    check line st (From _) = st |> modify (rememberStage line)  -- 遇到FROM指令，记录阶段
    check line st (User user)
      | not (isRoot user) = st |> modify forgetStage  -- 非root用户，清除阶段记录
      | otherwise = st |> modify (rememberLine line)  -- root用户，记录行号
    check _ st _ = st  -- 其他指令不做处理

    -- 判断是否为root用户
    isRoot user =
      Text.isPrefixOf "root:" user || Text.isPrefixOf "0:" user || user == "root" || user == "0"

    -- 标记失败，生成错误信息
    markFailures (State fails (Acc _ st)) = Map.foldl' (Seq.|>) fails (fmap makeFail st)
    markFailures st = failures st
    makeFail line = CheckFailure {..}  -- 生成CheckFailure记录
{-# INLINEABLE rule #-}

-- 记录阶段信息
rememberStage :: StageLine -> Acc -> Acc
rememberStage from (Acc _ m) = Acc from m  -- 更新阶段行号
rememberStage from Empty = Acc from Map.empty  -- 初始化阶段记录

-- 清除阶段信息
forgetStage :: Acc -> Acc
forgetStage (Acc from m) = Acc from (m |> Map.delete from)  -- 删除指定阶段
forgetStage Empty = Empty  -- 空状态保持不变

-- 记录行号信息
rememberLine :: StageLine -> Acc -> Acc
rememberLine line (Acc from m) = Acc from (m |> Map.insert from line)  -- 插入行号记录
rememberLine _ Empty = Empty  -- 空状态保持不变
