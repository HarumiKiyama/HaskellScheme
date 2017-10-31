{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal
  ( LispVal(..)
  , Eval(..)
  , IFunc(..)
  , EnvCtx
  , LispException(..)
  , showVal
  ) where

import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)

import           Control.Monad.Except
import           Control.Monad.Reader
type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval
  { unEval :: ReaderT EnvCtx IO a
  } deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc
           EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable, Eq)

data IFunc = IFunc
  { fn :: [LispVal] -> Eval LispVal
  }
instance Eq IFunc where
  (==) _ _ = False

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom)     -> atom
    (String str)    -> T.concat ["\"", str, "\""]
    (Number num)    -> T.pack $ show num
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
    Nil             -> "'()'"
    (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
    (Fun _)         -> "(internal function)"
    (Lambda _ _)    -> "(lambda function)"

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $ showVal <$> list

data LispException
  = NumArgs Integer
            [LispVal]
  | LengthOfList T.Text
                 Int
  | ExpectedList T.Text
  | TypeMismatch T.Text
                 LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String
  | IOError T.Text
  deriving (Typeable)
