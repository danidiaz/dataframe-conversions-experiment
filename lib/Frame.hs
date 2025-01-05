{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Frame (someFunc) where

import Type.Reflection (TypeRep, SomeTypeRep(..), eqTypeRep)
import Type.Reflection qualified
import Data.Map (Map)
import Data.Map qualified
import Data.Text (Text)
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Exception (Exception, throwIO)
import Data.Function ((&))

data Frame = Frame {
        opDict :: OpDict,
        convDict :: ConvDict,
        -- Does this need to be a mutable reference?
        colMapRef :: MVar (Map ColName SomeCol)
    }



newtype ColName = ColName Text  
    deriving newtype (Show, Eq, Ord)

data SomeCol where
    SomeCol :: TypeRep a -> Col a -> SomeCol

newtype Col a = Col [a]
    deriving newtype (Functor, Foldable)

data Op =
      Sum
    | Diff
    | Product
    deriving stock (Show, Eq, Ord)

data SomeOp where
    SomeOp :: TypeRep a -> (a -> a -> a) -> SomeOp

data OpDict = OpDict (Map (SomeTypeRep, Op) SomeOp)

performOp :: forall t. TypeRep t -> Col t -> Col t -> Op -> OpDict -> Maybe (Col t)
performOp tr (Col ls) (Col rs) op (OpDict dict) = 
    case Data.Map.lookup (SomeTypeRep tr, op) dict of
        Just (SomeOp tr' f) | Just Type.Reflection.HRefl <- tr `eqTypeRep` tr' -> 
            Just $ Col $ zipWith f ls rs
        _ -> Nothing

data SomeConv where
    SomeConv:: TypeRep a -> TypeRep b -> (a -> b) -> SomeConv

data ConvDict = ConvDict (Map (SomeTypeRep,SomeTypeRep) SomeConv)

performConv :: TypeRep l -> Col l -> TypeRep r -> Col r -> ConvDict -> Maybe (Either (Col l, Col l) (Col r, Col r))
performConv trL colL trR colR (ConvDict dict) = 
    case Data.Map.lookup (SomeTypeRep trL, SomeTypeRep trR) dict of
        Just (SomeConv trL' trR' f) 
            | Just Type.Reflection.HRefl <- trL `eqTypeRep` trL'
            , Just Type.Reflection.HRefl <- trR `eqTypeRep` trR' ->
                Just $ Right (f <$> colL, colR)
        _ -> 
            case Data.Map.lookup (SomeTypeRep trR, SomeTypeRep trL) dict of
                Just (SomeConv trR' trL' f) 
                    | Just Type.Reflection.HRefl <- trR `eqTypeRep` trR'
                    , Just Type.Reflection.HRefl <- trL `eqTypeRep` trL' ->
                        Just $ Left (colL, f <$> colR)
                _ -> Nothing

data OpDesc = OpDesc Op ColName ColName
    deriving stock (Show, Eq, Ord)

(+:) :: ColName -> ColName -> OpDesc
(+:) = OpDesc Product
infix 5 +:

(-:) :: ColName -> ColName -> OpDesc
(-:) = OpDesc Diff
infix 5 -:

(*:) :: ColName -> ColName -> OpDesc
(*:) = OpDesc Product
infix 5 *:

-- printCol :: ColName -> Frame -> IO ()
-- printCol = 


(.=) :: ColName -> OpDesc -> Frame -> IO ()
(.=) destColName (OpDesc op colNameL colNameR)  Frame {opDict, convDict, colMapRef} = do 
    modifyMVar_ colMapRef $ \colMap -> do
        let tryOp :: forall xt . TypeRep xt -> Col xt -> Col xt -> IO (Map ColName SomeCol)
            tryOp tr colL colR =
                            case opDict & performOp tr colL colR op of
                                Nothing -> do
                                    throwIO $ NoOpDefinition op (SomeTypeRep tr) 
                                Just newCol -> do
                                    pure $ Data.Map.insert destColName (SomeCol tr newCol) colMap
        case (Data.Map.lookup colNameL colMap, Data.Map.lookup colNameR colMap) of
            (Nothing, _) -> throwIO $ LeftColMissing colNameL
            (_, Nothing) -> throwIO $ RightColMissing colNameR
            (Just (SomeCol trL colL), Just (SomeCol trR colR)) ->
                if 
                    | Just Type.Reflection.HRefl <- trL `eqTypeRep` trR -> do
                        tryOp trL colL colR
                    | otherwise -> 
                        case convDict & performConv trL colL trR colR of
                            Just (Left (colL1, colL2)) -> do
                                tryOp trL colL1 colL2
                            Just (Right (colR1, colR2)) -> do
                                tryOp trR colR1 colR2
                            Nothing -> do
                                throwIO $ NoConvDefinition (SomeTypeRep trL) (SomeTypeRep trR)
infix 4 .=

-- frame & "foo" := "asd" _*_ "fff"

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data FrameOops = 
          LeftColMissing ColName
        | RightColMissing ColName
        | NoOpDefinition Op SomeTypeRep
        | NoConvDefinition SomeTypeRep SomeTypeRep
    deriving stock Show

instance Exception FrameOops