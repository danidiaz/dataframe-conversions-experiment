{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Frame
  ( Frame,
    createExampleFrame,
    describe,
    printCol,
    ColName,
    (.=),
    (+:),
    (-:),
    (*:),
    FrameError (..),
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (Exception, throwIO)
import Data.Foldable qualified
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified
import Data.String (IsString)
import Data.Text (Text)
import Type.Reflection (SomeTypeRep (..), TypeRep, Typeable, eqTypeRep, typeRep)
import Type.Reflection qualified

data Frame = Frame
  { opDict :: OpDict,
    convDict :: ConvDict,
    -- Does this need to be a mutable reference?
    colMapRef :: MVar (Map ColName SomeCol)
  }

createExampleFrame :: IO Frame
createExampleFrame = do
  let opDict =
        mconcat
          [ singletonOpDict @Int Sum (+),
            singletonOpDict @Int Diff (-),
            singletonOpDict @Int Product (*),
            singletonOpDict @Float Sum (+),
            singletonOpDict @Float Diff (-),
            singletonOpDict @Float Product (*),
            singletonOpDict @Double Sum (+),
            singletonOpDict @Double Diff (-),
            singletonOpDict @Double Product (*)
          ]
  let convDict =
        mconcat
          [ singletonConvDict @Int @Float realToFrac,
            singletonConvDict @Float @Double realToFrac
          ]
  let colMap =
        Data.Map.fromList
          [ ("inty", SomeCol (typeRep @Int) (Col [1, 2, 3])),
            ("floaty", SomeCol (typeRep @Float) (Col [1.0, 2.0, 3.0])),
            ("doubly", SomeCol (typeRep @Double) (Col [1.0, 2.0, 3.0]))
          ]
  colMapRef <- newMVar colMap
  pure Frame {opDict, convDict, colMapRef}

describe :: Frame -> IO ()
describe Frame {colMapRef} = do
  colMap <- readMVar colMapRef
  _ <-
    colMap
      & Data.Map.traverseWithKey
        ( \colName (SomeCol tr _) ->
            putStrLn $ show colName ++ " -> " ++ show tr
        )
  pure ()

printCol :: ColName -> Frame -> IO ()
printCol colName Frame {colMapRef} = do
  colMap <- readMVar colMapRef
  case (Data.Map.lookup colName colMap) of
    Nothing -> throwIO $ ColMissing colName
    Just (SomeCol _ (Col vs)) ->
      Data.Foldable.for_ vs print

newtype ColName = ColName Text
  deriving newtype (Show, Eq, Ord, IsString)

newtype Col a = Col [a]
  deriving newtype (Functor, Foldable)

data SomeCol where
  SomeCol :: (Show a) => TypeRep a -> Col a -> SomeCol

data Op
  = Sum
  | Diff
  | Product
  deriving stock (Show, Eq, Ord)

newtype OpDict = OpDict (Map (SomeTypeRep, Op) SomeOp)
  deriving newtype (Semigroup, Monoid)

data SomeOp where
  SomeOp :: (Show a) => TypeRep a -> (a -> a -> a) -> SomeOp

singletonOpDict :: forall a. (Show a, Typeable a) => Op -> (a -> a -> a) -> OpDict
singletonOpDict op f =
  OpDict $ Data.Map.singleton (SomeTypeRep (typeRep @a), op) (SomeOp (typeRep @a) f)

performOp :: forall t. TypeRep t -> Col t -> Col t -> Op -> OpDict -> Maybe (Col t)
performOp tr (Col ls) (Col rs) op (OpDict dict) =
  case Data.Map.lookup (SomeTypeRep tr, op) dict of
    Just (SomeOp tr' f)
      | Just Type.Reflection.HRefl <- tr `eqTypeRep` tr' ->
          Just $ Col $ zipWith f ls rs
    _ -> Nothing

newtype ConvDict = ConvDict (Map (SomeTypeRep, SomeTypeRep) SomeConv)
  deriving newtype (Semigroup, Monoid)

data SomeConv where
  SomeConv :: TypeRep a -> TypeRep b -> (a -> b) -> SomeConv

singletonConvDict :: forall a b. (Typeable a, Typeable b) => (a -> b) -> ConvDict
singletonConvDict f =
  ConvDict $ Data.Map.singleton (SomeTypeRep (typeRep @a), SomeTypeRep (typeRep @b)) (SomeConv (typeRep @a) (typeRep @b) f)

performConv :: TypeRep l -> Col l -> TypeRep r -> Col r -> ConvDict -> Maybe (Either (Col l, Col l) (Col r, Col r))
performConv trL colL trR colR (ConvDict dict) =
  case Data.Map.lookup (SomeTypeRep trL, SomeTypeRep trR) dict of
    Just (SomeConv trL' trR' f)
      | Just Type.Reflection.HRefl <- trL `eqTypeRep` trL',
        Just Type.Reflection.HRefl <- trR `eqTypeRep` trR' ->
          Just $ Right (f <$> colL, colR)
    _ ->
      case Data.Map.lookup (SomeTypeRep trR, SomeTypeRep trL) dict of
        Just (SomeConv trR' trL' f)
          | Just Type.Reflection.HRefl <- trR `eqTypeRep` trR',
            Just Type.Reflection.HRefl <- trL `eqTypeRep` trL' ->
              Just $ Left (colL, f <$> colR)
        _ -> Nothing

data OpDesc = OpDesc Op ColName ColName
  deriving stock (Show, Eq, Ord)

(+:) :: ColName -> ColName -> OpDesc
(+:) = OpDesc Sum

infix 5 +:

(-:) :: ColName -> ColName -> OpDesc
(-:) = OpDesc Diff

infix 5 -:

(*:) :: ColName -> ColName -> OpDesc
(*:) = OpDesc Product

infix 5 *:

(.=) :: ColName -> OpDesc -> Frame -> IO ()
(.=) destColName (OpDesc op colNameL colNameR) Frame {opDict, convDict, colMapRef} = do
  modifyMVar_ colMapRef $ \colMap -> do
    let tryOp :: forall xt. (Show xt) => TypeRep xt -> Col xt -> Col xt -> IO (Map ColName SomeCol)
        tryOp tr colL colR =
          case opDict & performOp tr colL colR op of
            Nothing -> do
              throwIO $ NoOpDefinition op (SomeTypeRep tr)
            Just newCol -> do
              pure $ Data.Map.insert destColName (SomeCol tr newCol) colMap
    case (Data.Map.lookup colNameL colMap, Data.Map.lookup colNameR colMap) of
      (Nothing, _) -> throwIO $ ColMissing colNameL
      (_, Nothing) -> throwIO $ ColMissing colNameR
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

data FrameError
  = ColMissing ColName
  | NoOpDefinition Op SomeTypeRep
  | NoConvDefinition SomeTypeRep SomeTypeRep
  deriving stock (Show)

instance Exception FrameError
