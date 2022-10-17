{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Internal.TH
-- Copyright   :  (C) 2018 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
--
-- The code for defining TwiML verbs is highly-repetitive and follows a pattern.
-- This module defines a little TwiML definition format and Template Haskell
-- function for generating this code.
--
-- For example, data types 'Pause', 'PauseF', and 'PauseAttributes' can all be
-- generated from the following definition:
--
-- @
-- Pause
--   attributes
--     duration, Natural, length
--   recursive
--   toXMLForGADT
--   toAttrsForAttributes
-- @
--
-- You should never need to import this module; it is only used during
-- compilation of the <index.html twiml> library.
-------------------------------------------------------------------------------
module Text.XML.Twiml.Internal.TH
  ( TwimlSpec(..)
  , example
  , exampleSpec
  , runTwimlSpecParser
  , twimlSpecToData
  , twimlSpecStringToData
  , s
  ) where

import Control.Monad
import Data.Char
import Data.Default
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parsec

example :: String
example = unlines [
  "Say",
  "  required",
  "    String",
  "  attributes",
  "    voice, Voice",
  "    loop, Bool",
  "  recursive" ]

exampleSpec :: TwimlSpec
exampleSpec = case runTwimlSpecParser example of
  Right exampleSpec' -> exampleSpec'
  Left  parseError   -> error $ show parseError

data TwimlSpec = TwimlSpec
  { twimlName  :: String
  , parameters :: [Parameters]
  , recursive  :: Bool
  , toXMLForGADT :: Bool
  , toAttrsForAttributes :: Bool
  } deriving Show

instance Default TwimlSpec where
  def = TwimlSpec def def False False False

data Parameters
  = Required { getRequiredTypes :: [String] }
  | Attributes { getAttributes :: [Attribute] }
  deriving Show

data Attribute = Attribute
  { attributeName :: String
  , attributeType :: String
  , overrideName  :: Maybe String
  } deriving Show

isRequired :: Parameters -> Bool
isRequired (Required _) = True
isRequired _ = False

isAttributes :: Parameters -> Bool
isAttributes (Attributes _) = True
isAttributes _ = False

getAllRequired :: [Parameters] -> [String]
getAllRequired = concatMap getRequiredTypes . filter isRequired

getAllAttributes :: [Parameters] -> [Attribute]
getAllAttributes = concatMap getAttributes . filter isAttributes

hasAttributes :: TwimlSpec -> Bool
hasAttributes = not . null . getAllAttributes . parameters

attributeToVarStrictType :: (String -> String) -> Attribute -> VarStrictType
attributeToVarStrictType f Attribute{..} =
  ( mkName $ f attributeName
#if MIN_VERSION_template_haskell(2,11,0)
  , Bang NoSourceUnpackedness SourceStrict
#else
  , IsStrict
#endif
  , AppT (ConT $ mkName "Maybe") (ConT $ mkName attributeType)
  )

parametersToVarStrictTypes :: (String -> String) -> [Parameters] -> [VarStrictType]
parametersToVarStrictTypes f = map (attributeToVarStrictType f) . getAllAttributes

-- rnfI :: Int -> Exp
-- rnfI i = rnfNames . take i $ map (mkName . return) ['a'..'z']

-- rnfNames :: [Name] -> Exp
-- rnfNames [] = TupE []
-- rnfNames [a] = rnfName a
-- rnfNames (a:as) = AppE (AppE (VarE $ mkName "seq") (rnfName a)) (rnfNames as)

-- rnfName :: Name -> Exp
-- rnfName name = AppE (VarE $ mkName "rnf") (VarE name)

specToGADTName :: TwimlSpec -> Name
specToGADTName TwimlSpec{..} = mkName $ twimlName ++ "F"

specToAttributesName :: TwimlSpec -> Name
specToAttributesName TwimlSpec{..} = mkName $ twimlName ++ "Attributes"

specToGADTArity :: TwimlSpec -> Int
specToGADTArity spec@(TwimlSpec{..}) = length (getAllRequired parameters) + (if hasAttributes spec then 1 else 0) + (if recursive then 1 else 0)

specToGADTNames :: TwimlSpec -> [Name]
specToGADTNames spec@(TwimlSpec{}) =
  take (specToGADTArity spec) $ map (mkName . return) ['a'..'z']

specToGADTAttributesName :: TwimlSpec -> Maybe Name
specToGADTAttributesName spec@(TwimlSpec{..}) = go $ zip parameters $ specToGADTNames spec where
  go [] = Nothing
  go ((Required _, _):rest) = go rest
  go ((Attributes _, name):_) = Just name

specToGADTChildName :: TwimlSpec -> Maybe Name
specToGADTChildName spec@(TwimlSpec{..}) = go $ zip parameters $ specToGADTNames spec where
  go [] = Nothing
  go ((Required _, name):_) = Just name
  go ((Attributes _, _):rest) = go rest

specToGADTPat :: TwimlSpec -> Pat
#if MIN_VERSION_template_haskell(2,19,0)
specToGADTPat spec@(TwimlSpec{}) = ConP (specToGADTName spec) [] varPs where
  varPs = map VarP $ specToGADTNames spec
#else
specToGADTPat spec@(TwimlSpec{}) = ConP (specToGADTName spec) varPs where
  varPs = map VarP $ specToGADTNames spec
#endif

specToAttributesListE :: TwimlSpec -> Exp
specToAttributesListE (TwimlSpec{..}) = ListE . map go $ getAllAttributes parameters where
  go (Attribute{..}) =
    let name = LitE . StringL $ fromMaybe attributeName overrideName
    in  AppE (AppE (VarE $ mkName "makeAttr") name) (VarE . mkName $ makeAttr attributeName)
  attrPrefix = '_' : map toLower twimlName
  makeAttr (a:ttrName) = attrPrefix ++ toUpper a : ttrName
  makeAttr _ = error "Unsupported"

specToToXML :: TwimlSpec -> Exp
specToToXML spec@(TwimlSpec{..}) = UInfixE (AppE (AppE (AppE (VarE $ mkName "makeElement") (LitE $ StringL twimlName)) (AppE (VarE $ mkName "toSomeNode") child)) attributesE) (ConE $ mkName ":") next where
  child = maybe (TupE []) VarE $ specToGADTChildName spec
  attributesE = maybe (ListE []) (AppE (VarE $ mkName "toAttrs") . VarE) $ specToGADTAttributesName spec
  next = if recursive
    then AppE (VarE $ mkName "toXML") (VarE . last $ specToGADTNames spec)
    else ListE []

specToStrictTypes :: TwimlSpec -> [StrictType]
#if MIN_VERSION_template_haskell(2,11,0)
specToStrictTypes spec@(TwimlSpec{..}) = go parameters ++ [(Bang NoSourceUnpackedness NoSourceStrictness, VarT $ mkName "a") | recursive] where
#else
specToStrictTypes spec@(TwimlSpec{..}) = go parameters ++ [(NotStrict, VarT $ mkName "a") | recursive] where
#endif
  go [] = []
  go (Required   as :bs) = map stringToStrictType as ++ go bs
#if MIN_VERSION_template_haskell(2,11,0)
  go (Attributes _  :bs) = (Bang NoSourceUnpackedness NoSourceStrictness, ConT $ specToAttributesName spec) : go bs
#else
  go (Attributes _  :bs) = (NotStrict, ConT $ specToAttributesName spec) : go bs
#endif
#if MIN_VERSION_template_haskell(2,11,0)
  stringToStrictType a = (Bang NoSourceUnpackedness NoSourceStrictness, ConT $ mkName a)
#else
  stringToStrictType a = (NotStrict, ConT $ mkName a)
#endif

-- gadtToDefExp :: TwimlSpec -> [Parameters] -> Exp
-- gadtToDefExp spec@(TwimlSpec{..}) = go (ConE $ specToGADTName spec) . foldr ((+) . count) (if recursive then 1 else 0) where
--   go conE 0 = conE
--   go conE n = go (AppE conE defE) (n-1)
--   defE = VarE $ mkName "def"
--   count (Required r) = length r
--   count _ = 1

attributesToDefExp :: Exp -> [Parameters] -> Exp
attributesToDefExp conE = go conE . length . getAllAttributes where
  go conE 0 = conE
  go conE n = go (AppE conE defE) (n-1)
  defE = VarE $ mkName "def"

instance Default Attribute where
  def = Attribute def def def

parseTwimlSpec :: Parsec String () TwimlSpec
parseTwimlSpec = do
  twimlName  <- parseTwimlName
  parameters <- parseParameters
  recursive  <- option False $ try parseRecursive
  toXMLForGADT <- option False parseToXMLForGADT
  toAttrsForAttributes <- option False parseToAttrsForAttributes
  eof
  return $ TwimlSpec twimlName parameters recursive toXMLForGADT toAttrsForAttributes

parseTwimlName :: Parsec String () String
parseTwimlName = many1 letter <* newline

parseParameters :: Parsec String () [Parameters]
parseParameters = many (try parseRequiredSection <|> try parseAttributesSection)

parseRequiredSection :: Parsec String () Parameters
parseRequiredSection = do
  string "  required"; newline
  Required <$> many (try parseRequired)

parseRequired :: Parsec String () String
parseRequired =
  string "    " >> many1 (noneOf "\n") <* newline

parseAttributesSection :: Parsec String () Parameters
parseAttributesSection = do
  string "  attributes"; newline
  Attributes <$> many (try parseAttribute)

parseAttribute :: Parsec String () Attribute
parseAttribute = do
  string "    "
  abc <- many1 (noneOf ",\n") `sepBy` string ", " <* newline
  case abc of
    [a,b]   -> return $ Attribute a b Nothing
    a:b:[c] -> return . Attribute a b $ Just c
    _       -> mzero

parseRecursive :: Parsec String () Bool
parseRecursive =
  const True <$> string "  recursive" <* newline

parseToXMLForGADT :: Parsec String () Bool
parseToXMLForGADT =
  const True <$> string "  toXMLForGADT" <* newline

parseToAttrsForAttributes :: Parsec String () Bool
parseToAttrsForAttributes =
  const True <$> string "  toAttrsForAttributes" <* newline

runTwimlSpecParser :: String -> Either ParseError TwimlSpec
runTwimlSpecParser = runParser parseTwimlSpec () ""

s :: QuasiQuoter
s = QuasiQuoter {
  quoteExp = stringE . trim
, quotePat = undefined
, quoteType = undefined
, quoteDec = undefined
}

trim :: String -> String
trim = trimTail . dropWhile isSpace

trimTail :: String -> String
trimTail "" = "\n"
trimTail s = take (lastNonBlank s) s ++ "\n"
  where lastNonBlank = (+1) . fst . foldl acc (0, 0)
        acc (l, n) c | isSpace c = (l, n + 1)
                     | otherwise = (n, n + 1)

twimlSpecStringToData :: String -> DecsQ
twimlSpecStringToData str = case runTwimlSpecParser str of
  Right twimlSpec -> twimlSpecToData twimlSpec
  Left  msg       -> error $ show msg

-- | Create an indexed GADT from a name. For example, given "Foo", this
-- generates (roughly)
--
-- @@
-- data Foo
-- data FooF i a where
--   FooF :: a -> FooF '[Foo] a
-- @@
twimlSpecToData :: TwimlSpec -> DecsQ
twimlSpecToData spec@(TwimlSpec{..}) = pure $
    [ emptyDataDecl
    , gadt
--    , deriveDataForGADT
--    , instanceDefaultForGADT
--    , deriveEqForGADT
    , deriveFunctorForGADT
    , instanceFunctor1ForGADT
--    , instanceNFDataForGADT
--    , deriveOrdForGADT
--    , deriveReadForGADT
    , deriveShowForGADT
--    , instanceToXMLForGADT
    , attributes
    , instanceDefaultForAttributes
    ]
    ++ [instanceToXMLForGADT | toXMLForGADT]
    ++ [instanceToAttrsForAttributes | toAttrsForAttributes]
  where
    conName = mkName twimlName

    -- | @data Foo@
#if MIN_VERSION_template_haskell(2,12,0)
    emptyDataDecl = DataD [] conName [] Nothing [] [DerivClause Nothing []]
#else
#if MIN_VERSION_template_haskell(2,11,0)
    emptyDataDecl = DataD [] conName [] Nothing [] []
#else
    emptyDataDecl = DataD [] conName [] [] []
#endif
#endif

    -- | Type variables @i :: [*]@ and @a@
    i' = mkName "i"
    a' = mkName "a"
    i = VarT i'
    a = VarT a'
#if MIN_VERSION_template_haskell(2,19,0)
    tyVarBndrs = [KindedTV i' () $ AppT ListT StarT, PlainTV a' ()]
#else
    tyVarBndrs = [KindedTV i' $ AppT ListT StarT, PlainTV a']
#endif
    -- | @Proxy@
    proxy = ConT $ mkName "Proxy"

    -- | @Proxy i@
    proxyI = AppT proxy i

    -- | @'[Foo]@
    list = AppT (AppT PromotedConsT (ConT conName)) PromotedNilT

    -- | @Proxy '[Foo]@
    proxyList = AppT proxy list

    -- | @Proxy i ~ Proxy '[Foo]@
    --
    -- Unfortunately, this is the only way I know of to constrain the kind of
    -- @i@.
    cxt' = [AppT (AppT EqualityT proxyI) proxyList]

    conNameF = mkName $ twimlName ++ "F"
    con = ForallC [] cxt'
        . NormalC conNameF $ specToStrictTypes spec

    -- | @data FooF i a where FooF :: a -> FooF '[Foo] a@
#if MIN_VERSION_template_haskell(2,12,0)
    gadt = DataD [] conNameF tyVarBndrs Nothing [con] [DerivClause Nothing []]
#else
#if MIN_VERSION_template_haskell(2,11,0)
    gadt = DataD [] conNameF tyVarBndrs Nothing [con] []
#else
    gadt = DataD [] conNameF tyVarBndrs [con] []
#endif
#endif

    dataN = mkName "Data"
    -- dataC = ConT dataN

    defaultN = mkName "Default"
    defaultC = ConT defaultN

    -- enumN = mkName "Enum"
    -- enumC = ConT enumN

    eqN = mkName "Eq"
    -- eqC = ConT eqN

    functorN = mkName "Functor"
    functorC = ConT functorN

    functor1N = mkName "Functor1"
    functor1C = ConT functor1N

    genericN = mkName "Generic"
    -- genericC = ConT genericN

    nfdataN = mkName "NFData"
    -- nfdataC = ConT nfdataN

    ordN = mkName "Ord"
    -- ordC = ConT ordN

    readN = mkName "Read"
    -- readC = ConT readN

    showN = mkName "Show"
    showC = ConT showN

    toAttrsN = mkName "ToAttrs"
    toAttrsC = ConT toAttrsN

    toXMLN = mkName "ToXML"
    toXMLC = ConT toXMLN

    -- | @instance Default a => Default (FooF i a) where def = FooF def ...@
#if MIN_VERSION_template_haskell(2,11,0)
--     instanceDefaultForGADT = InstanceD Nothing [AppT defaultC a] (AppT defaultC (AppT (AppT (ConT conNameF) list) a)) [ValD (VarP $ mkName "def") (NormalB $ gadtToDefExp spec parameters) []]
#else
--     instanceDefaultForGADT = InstanceD [AppT defaultC a] (AppT defaultC (AppT (AppT (ConT conNameF) list) a)) [ValD (VarP $ mkName "def") (NormalB $ gadtToDefExp spec parameters) []]
#endif

    -- | @deriving instance Data a => Data (FooF i a)@
#if MIN_VERSION_template_haskell(2,12,0)
--     deriveDataForGADT = StandaloneDerivD Nothing [AppT dataC a] $ AppT dataC (AppT (AppT (ConT conNameF) list) a)
#else
--     deriveDataForGADT = StandaloneDerivD [AppT dataC a] $ AppT dataC (AppT (AppT (ConT conNameF) list) a)
#endif

    -- | @deriving instance Eq a => Eq (FooF i a)@
#if MIN_VERSION_template_haskell(2,12,0)
--     deriveEqForGADT = StandaloneDerivD Nothing [AppT eqC a] $ AppT eqC (AppT (AppT (ConT conNameF) i) a)
#else
--     deriveEqForGADT = StandaloneDerivD [AppT eqC a] $ AppT eqC (AppT (AppT (ConT conNameF) i) a)
#endif

    -- | @deriving instance Functor (FooF i)@
#if MIN_VERSION_template_haskell(2,12,0)
    deriveFunctorForGADT = StandaloneDerivD Nothing [] $ AppT functorC (AppT (ConT conNameF) i)
#else
    deriveFunctorForGADT = StandaloneDerivD [] $ AppT functorC (AppT (ConT conNameF) i)
#endif

    -- | @instance Functor1 FooF where fmap1 = fmap@
#if MIN_VERSION_template_haskell(2,11,0)
    instanceFunctor1ForGADT = InstanceD Nothing [] (AppT functor1C $ ConT conNameF) [ValD (VarP $ mkName "fmap1") (NormalB . VarE $ mkName "fmap") []]
#else
    instanceFunctor1ForGADT = InstanceD [] (AppT functor1C $ ConT conNameF) [ValD (VarP $ mkName "fmap1") (NormalB . VarE $ mkName "fmap") []]
#endif

    -- | @instance NFData a => NFData (FooF i a) where rnf (FooF a ...) = rnf a `seq` ...@
#if MIN_VERSION_template_haskell(2,11,0)
--     instanceNFDataForGADT = InstanceD Nothing [AppT nfdataC a] (AppT nfdataC (AppT (AppT (ConT conNameF) list) a)) [FunD (mkName "rnf") [Clause [specToGADTPat spec] (NormalB . rnfI $ specToGADTArity spec) []]]
#else
--     instanceNFDataForGADT = InstanceD [AppT nfdataC a] (AppT nfdataC (AppT (AppT (ConT conNameF) list) a)) [FunD (mkName "rnf") [Clause [specToGADTPat spec] (NormalB . rnfI $ specToGADTArity spec) []]]
#endif

    -- | @deriving instance Ord a => Ord (FooF i a)@
#if MIN_VERSION_template_haskell(2,12,0)
--     deriveOrdForGADT = StandaloneDerivD Nothing [AppT ordC a] $ AppT ordC (AppT (AppT (ConT conNameF) i) a)
#else
--     deriveOrdForGADT = StandaloneDerivD [AppT ordC a] $ AppT ordC (AppT (AppT (ConT conNameF) i) a)
#endif

    -- | @deriving instance Read a => Read (FooF i a)@
#if MIN_VERSION_template_haskell(2,12,0)
--     deriveReadForGADT = StandaloneDerivD Nothing [AppT readC a] $ AppT readC (AppT (AppT (ConT conNameF) list) a)
#else
--     deriveReadForGADT = StandaloneDerivD [AppT readC a] $ AppT readC (AppT (AppT (ConT conNameF) list) a)
#endif

    -- | @deriving instance Show a => Show (FooF i a)@
#if MIN_VERSION_template_haskell(2,12,0)
    deriveShowForGADT = StandaloneDerivD Nothing [AppT showC a] $ AppT showC (AppT (AppT (ConT conNameF) i) a)
#else
    deriveShowForGADT = StandaloneDerivD [AppT showC a] $ AppT showC (AppT (AppT (ConT conNameF) i) a)
#endif

    -- | @instance ToXML a => ToXML (FooF i a) where toXML (FooF a ...) = makeElement "Foo" a ...@
#if MIN_VERSION_template_haskell(2,11,0)
    instanceToXMLForGADT = InstanceD Nothing [AppT toXMLC a | recursive] (AppT toXMLC (AppT (AppT (ConT conNameF) i) a))
#else
    instanceToXMLForGADT = InstanceD [AppT toXMLC a | recursive] (AppT toXMLC (AppT (AppT (ConT conNameF) i) a))
#endif
      [FunD (mkName "toXML") [Clause [specToGADTPat spec] (NormalB $ specToToXML spec) []]]

    attrPrefix = '_' : map toLower twimlName
    makeAttr (a:ttrName) = attrPrefix ++ toUpper a : ttrName
    makeAttr _ = error "Unsupported"
    attributesName = specToAttributesName spec

    -- | @data FooAttributes = FooAttributes{..} deriving (Data, Eq, Ord, Read, Show)@
    --
    -- All record fields should be camelCased and prefixed with "_foo".
#if MIN_VERSION_template_haskell(2,12,0)
    attributes = DataD [] attributesName [] Nothing [RecC attributesName (parametersToVarStrictTypes makeAttr parameters)] . pure . DerivClause Nothing $ ConT <$> [dataN, eqN, genericN, nfdataN, ordN, readN, showN]
#else
#if MIN_VERSION_template_haskell(2,11,0)
    attributes = DataD [] attributesName [] Nothing [RecC attributesName (parametersToVarStrictTypes makeAttr parameters)] $ ConT <$> [dataN, eqN, genericN, nfdataN, ordN, readN, showN]
#else
    attributes = DataD [] attributesName [] [RecC attributesName (parametersToVarStrictTypes makeAttr parameters)] [dataN, eqN, genericN, nfdataN, ordN, readN, showN]
#endif
#endif

    -- | @instance Default FooAttributes where def = FooAttributes def ...@
#if MIN_VERSION_template_haskell(2,11,0)
    instanceDefaultForAttributes = InstanceD Nothing [] (AppT defaultC $ ConT attributesName) [ValD (VarP $ mkName "def") (NormalB $ attributesToDefExp (ConE attributesName) parameters) []]
#else
    instanceDefaultForAttributes = InstanceD [] (AppT defaultC $ ConT attributesName) [ValD (VarP $ mkName "def") (NormalB $ attributesToDefExp (ConE attributesName) parameters) []]
#endif

#if MIN_VERSION_template_haskell(2,11,0)
    instanceToAttrsForAttributes = InstanceD Nothing [] (AppT toAttrsC $ ConT attributesName) [ValD (VarP $ mkName "toAttrs") (NormalB (AppE (AppE (VarE $ mkName "flip") (VarE $ mkName "makeAttrs")) (specToAttributesListE spec))) []]
#else
    instanceToAttrsForAttributes = InstanceD [] (AppT toAttrsC $ ConT attributesName) [ValD (VarP $ mkName "toAttrs") (NormalB (AppE (AppE (VarE $ mkName "flip") (VarE $ mkName "makeAttrs")) (specToAttributesListE spec))) []]
#endif
