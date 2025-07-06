{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pact code formatter based on "A Prettier Printer" by Philip Wadler
-- This module provides standalone formatting functionality for Pact code
module Pact.Core.Formatter
  ( -- * Main API
    formatPactFile
  , formatPactCode
  , formatPactCodeWithConfig
  -- * Configuration
  , FormatterConfig(..)
  , defaultFormatterConfig
  -- * Formatting functions
  , formatTopLevel
  , formatModule
  , formatInterface
  , formatDef
  , formatExpr
  -- * Pretty printing internals
  , Doc(..)
  , pretty
  , renderDoc
  ) where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import System.Exit (exitFailure)

import Pact.Core.Syntax.Lexer (lexer)
import Pact.Core.Syntax.Parser (parseReplProgram)
import Pact.Core.Syntax.ParseTree as PT
import Pact.Core.Info (SpanInfo)
import Pact.Core.Literal
import Pact.Core.Names hiding (Arg)
import Pact.Core.Type (PrimType(..))
import Pact.Core.Errors (PactErrorI)

-- | Formatter configuration
data FormatterConfig = FormatterConfig
  { cfgIndentSize :: Int
  , cfgMaxLineWidth :: Int
  , cfgSpaceBetweenDefinitions :: Bool
  , cfgSpaceInsideModules :: Bool
  , cfgAlignBindings :: Bool
  , cfgAlignLetBindings :: Bool
  , cfgBreakLongFunctionCalls :: Bool
  , cfgBreakBeforeClosingParen :: Bool
  } deriving (Show, Eq)

-- | Default formatting configuration
defaultFormatterConfig :: FormatterConfig
defaultFormatterConfig = FormatterConfig
  { cfgIndentSize = 2
  , cfgMaxLineWidth = 80
  , cfgSpaceBetweenDefinitions = True
  , cfgSpaceInsideModules = True
  , cfgAlignBindings = True
  , cfgAlignLetBindings = True
  , cfgBreakLongFunctionCalls = True
  , cfgBreakBeforeClosingParen = False
  }

--------------------------------------------------
-- Pretty Printing Core based on Wadler's algorithm
--------------------------------------------------

-- | Document type representing pretty printer combinators
data Doc
  = Empty
  | Char Char
  | Text !Int Text  -- Int is length for efficiency
  | Line !Bool      -- Bool indicates if this can be flattened to space
  | Cat Doc Doc
  | Nest !Int Doc
  | Union Doc Doc   -- Choice between two layouts
  | Column (Int -> Doc)  -- Access to current column
  | Nesting (Int -> Doc) -- Access to current nesting level

instance Semigroup Doc where
  (<>) = Cat

instance Monoid Doc where
  mempty = Empty

-- Smart constructors
text :: Text -> Doc
text t = Text (T.length t) t

char :: Char -> Doc
char = Char

line :: Doc
line = Line False

nest :: Int -> Doc -> Doc
nest = Nest

group :: Doc -> Doc
group x = Union (flatten x) x
  where
    flatten :: Doc -> Doc
    flatten = \case
      Empty -> Empty
      Char c -> Char c
      Text n s -> Text n s
      Line b -> if b then Char ' ' else Line False
      Cat x' y' -> Cat (flatten x') (flatten y')
      Nest i x' -> Nest i (flatten x')
      Union x' _ -> flatten x'
      Column f -> Column (flatten . f)
      Nesting f -> Nesting (flatten . f)

-- | Render a document to Text with a given width
renderDoc :: Int -> Doc -> Text
renderDoc width doc = T.pack $ best 0 0 [(0, doc)]
  where
    best :: Int -> Int -> [(Int, Doc)] -> String
    best _ _ [] = ""
    best col _ ((i, d):ds) = case d of
      Empty -> best col i ds
      Char c -> c : best (col + 1) i ds
      Text n s -> T.unpack s ++ best (col + n) i ds
      Line _ -> '\n' : replicate i ' ' ++ best i i ds
      Cat x y -> best col i ((i, x) : (i, y) : ds)
      Nest j x -> best col i ((i + j, x) : ds)
      Union x y -> better col i ds (best col i ((i, x) : ds)) (best col i ((i, y) : ds))
      Column f -> best col i ((i, f col) : ds)
      Nesting f -> best col i ((i, f i) : ds)
    
    better :: Int -> Int -> [(Int, Doc)] -> String -> String -> String
    better col _ _ x y = if fits (width - col) x then x else y
    
    fits :: Int -> String -> Bool
    fits w _ | w < 0 = False
    fits _ [] = True
    fits _ ('\n':_) = True
    fits w (_:xs) = fits (w - 1) xs

-- | Render with default width
pretty :: Doc -> Text
pretty = renderDoc 80

-- Combinators
hsep :: [Doc] -> Doc
hsep = fold . intersperse (char ' ')

vsep :: [Doc] -> Doc
vsep = fold . intersperse line

softline :: Doc
softline = group line


vcat :: [Doc] -> Doc
vcat = fold . intersperse (Line False)

parens :: Doc -> Doc
parens d = char '(' <> d <> char ')'

brackets :: Doc -> Doc
brackets d = char '[' <> d <> char ']'

braces :: Doc -> Doc
braces d = char '{' <> d <> char '}'

dquotes :: Doc -> Doc
dquotes d = char '"' <> d <> char '"'

comma :: Doc
comma = char ','

space :: Doc
space = char ' '

align :: Doc -> Doc
align d = Column $ \k -> Nesting $ \i -> nest (k - i) d


punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate p (x:xs) = (x <> p) : punctuate p xs

--------------------------------------------------
-- Pact-specific formatting
--------------------------------------------------

-- | Format configuration environment
data FormatEnv = FormatEnv
  { fmtConfig :: FormatterConfig
  }

-- | Format a Pact file
formatPactFile :: FilePath -> IO ()
formatPactFile path = do
  content <- T.readFile path
  case formatPactCode content of
    Left err -> do
      putStrLn $ "Error formatting file: " ++ show err
      exitFailure
    Right formatted -> T.writeFile path formatted

-- | Format Pact code with default configuration
formatPactCode :: Text -> Either PactErrorI Text
formatPactCode = formatPactCodeWithConfig defaultFormatterConfig

-- | Format Pact code with specific configuration
formatPactCodeWithConfig :: FormatterConfig -> Text -> Either PactErrorI Text
formatPactCodeWithConfig config content = do
  tokens <- lexer content
  replProgram <- parseReplProgram tokens
  let strippedProgram = map stripSpanInfo replProgram
      env = FormatEnv config
      docs = mapMaybe (formatReplTopLevel env) strippedProgram
      separated = if cfgSpaceBetweenDefinitions config
                  then intersperse (line <> line) docs
                  else intersperse line docs
      finalDoc = vcat separated
  pure $ renderDoc (cfgMaxLineWidth config) finalDoc

stripSpanInfo :: ReplTopLevel SpanInfo -> ReplTopLevel ()
stripSpanInfo = \case
  RTLTopLevel tl -> RTLTopLevel (fmap (const ()) tl)
  RTLDefun d -> RTLDefun (fmap (const ()) d)
  RTLDefConst d -> RTLDefConst (fmap (const ()) d)

formatReplTopLevel :: FormatEnv -> ReplTopLevel () -> Maybe Doc
formatReplTopLevel env = \case
  RTLTopLevel tl -> Just $ formatTopLevel env tl
  RTLDefun d -> Just $ formatDefun env d
  RTLDefConst d -> Just $ formatDefConst env d

formatTopLevel :: FormatEnv -> TopLevel () -> Doc
formatTopLevel env = \case
  TLModule m -> formatModule env m
  TLInterface i -> formatInterface env i
  TLTerm e -> formatExpr env e
  TLUse imp -> formatImport env imp

formatModule :: FormatEnv -> Module () -> Doc
formatModule env (Module name gov exts defs anns _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "module" <> space <> text name <> space <> formatGovernance gov
      annsDoc = if null anns then mempty else line <> vcat (map formatPactAnn anns)
      extsDoc = if null exts then mempty else line <> vcat (map (formatExtDecl env) exts)
      defsDoc = line <> formatDefs env (NE.toList defs)
      bodyDoc = nest indent' (annsDoc <> extsDoc <> defsDoc)
  in parens (headerDoc <> bodyDoc)

formatInterface :: FormatEnv -> Interface () -> Doc
formatInterface env (Interface name defns imports anns _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "interface" <> space <> text name
      annsDoc = if null anns then mempty else line <> vcat (map formatPactAnn anns)
      importsDoc = if null imports then mempty else line <> vcat (map (formatImport env) imports)
      defnsDoc = if null defns then mempty else line <> vcat (intersperse line $ map (formatIfDef env) defns)
      bodyDoc = nest indent' (annsDoc <> importsDoc <> defnsDoc)
  in parens (headerDoc <> bodyDoc)

formatGovernance :: Governance -> Doc
formatGovernance = \case
  KeyGov t -> dquotes (text t)
  CapGov t -> text t

formatPactAnn :: PactAnn () -> Doc
formatPactAnn = \case
  PactDoc _ t -> text "@doc" <> space <> dquotes (text t)
  PactModel props ->
    text "@model" <> space <> brackets (space <> align (vsep (map formatPropertyExpr props)) <> space)

formatPropertyExpr :: PropertyExpr () -> Doc
formatPropertyExpr = \case
  PropAtom pn _ -> formatParsedName pn
  PropKeyword kw _ -> formatPropKeyword kw
  PropDelim delim _ -> formatPropDelim delim
  PropSequence [] _ -> text "()"
  PropSequence (x:xs) _ -> parens (hsep $ map formatPropertyExpr (x:xs))
  PropConstant lit _ -> formatLiteral lit

formatPropKeyword :: PropKeyword -> Doc
formatPropKeyword = \case
  KwLet -> text "let"
  KwLambda -> text "lambda"
  KwDefProperty -> text "defproperty"

formatPropDelim :: PropDelim -> Doc
formatPropDelim = \case
  DelimLBracket -> text "["
  DelimRBracket -> text "]"
  DelimLBrace -> text "{"
  DelimRBrace -> text "}"
  DelimComma -> text ","
  DelimColon -> text ":"
  DelimWalrus -> text ":="

formatExtDecl :: FormatEnv -> ExtDecl () -> Doc
formatExtDecl env = \case
  ExtBless t _ -> parens (text "bless" <> space <> dquotes (text t))
  ExtImport imp -> formatImport env imp
  ExtImplements mn _ -> parens (text "implements" <> space <> formatModuleName mn)

formatImport :: FormatEnv -> Import () -> Doc
formatImport _ (Import mn mhash mimports _) =
  let baseDoc = text "use" <> space <> formatModuleName mn
      hashDoc = case mhash of
        Nothing -> mempty
        Just h -> space <> dquotes (text h)
      importsDoc = case mimports of
        Nothing -> mempty
        Just imps -> space <> brackets (hsep $ map text imps)
  in parens (baseDoc <> hashDoc <> importsDoc)

formatDefs :: FormatEnv -> [Def ()] -> Doc
formatDefs env defs =
  let spaceBetween = cfgSpaceInsideModules (fmtConfig env)
      docs = map (formatDef env) defs
  in if spaceBetween
     then vcat (intersperse line docs)
     else vcat docs

formatDef :: FormatEnv -> Def () -> Doc
formatDef env = \case
  Dfun d -> formatDefun env d
  DConst d -> formatDefConst env d
  DCap d -> formatDefCap env d
  DSchema d -> formatDefSchema env d
  DTable d -> formatDefTable env d
  DPact d -> formatDefPact env d

formatDefun :: FormatEnv -> Defun () -> Doc
formatDefun env (Defun spec args body anns _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "defun" <> space <> formatMArg spec <> space <> formatArgs args
      annsDoc = if null anns then mempty else line <> vcat (map formatPactAnn anns)
      bodyDoc = line <> formatBody env body
      contentDoc = nest indent' (annsDoc <> bodyDoc)
  in parens (headerDoc <> contentDoc)

formatDefConst :: FormatEnv -> DefConst () -> Doc
formatDefConst env (DefConst spec term mdoc _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "defconst" <> space <> formatMArg spec
      docDoc = case mdoc of
        Nothing -> mempty
        Just (doc, _) -> line <> dquotes (text doc)
      termDoc = line <> formatExpr env term
      contentDoc = nest indent' (docDoc <> termDoc)
  in parens (headerDoc <> contentDoc)

formatDefCap :: FormatEnv -> DefCap () -> Doc
formatDefCap env (DefCap spec args body anns mmeta _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "defcap" <> space <> formatMArg spec <> space <> formatArgs args
      annsDoc = if null anns then mempty else line <> vcat (map formatPactAnn anns)
      metaDoc = case mmeta of
        Nothing -> mempty
        Just meta -> line <> formatDCapMeta meta
      bodyDoc = line <> formatBody env body
      contentDoc = nest indent' (annsDoc <> metaDoc <> bodyDoc)
  in parens (headerDoc <> contentDoc)

formatDCapMeta :: DCapMeta -> Doc
formatDCapMeta = \case
  DefEvent -> text "@event"
  DefManaged Nothing -> text "@managed"
  DefManaged (Just (n, pn)) -> text "@managed" <> space <> text n <> space <> formatParsedName pn

formatDefSchema :: FormatEnv -> DefSchema () -> Doc
formatDefSchema env (DefSchema name args anns _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "defschema" <> space <> text name
      annsDoc = if null anns then mempty else line <> vcat (map formatPactAnn anns)
      argsDoc = if null args then mempty else line <> vcat (map formatArg args)
      contentDoc = nest indent' (annsDoc <> argsDoc)
  in parens (headerDoc <> contentDoc)

formatDefTable :: FormatEnv -> DefTable () -> Doc
formatDefTable _ (DefTable name schema mdoc _) =
  let schemaDoc = text name <> char ':' <> braces (formatParsedName schema)
      docDoc = case mdoc of
        Nothing -> mempty
        Just (doc, _) -> line <> dquotes (text doc)
  in parens (text "deftable" <> space <> schemaDoc <> docDoc)

formatDefPact :: FormatEnv -> DefPact () -> Doc
formatDefPact env (DefPact spec args steps anns _) =
  let indent' = cfgIndentSize (fmtConfig env)
      headerDoc = text "defpact" <> space <> formatMArg spec <> space <> formatArgs args
      annsDoc = if null anns then mempty else line <> vcat (map formatPactAnn anns)
      stepsDoc = line <> vcat (map (formatPactStep env) (NE.toList steps))
      contentDoc = nest indent' (annsDoc <> stepsDoc)
  in parens (headerDoc <> contentDoc)

formatPactStep :: FormatEnv -> PactStep () -> Doc
formatPactStep env = \case
  Step _ expr manns ->
    let annsDoc = case manns of
          Nothing -> mempty
          Just anns -> space <> formatPactAnn (PactModel anns)
    in parens (text "step" <> space <> formatExpr env expr <> annsDoc)
  StepWithRollback _ expr rollback manns ->
    let annsDoc = case manns of
          Nothing -> mempty
          Just anns -> space <> formatPactAnn (PactModel anns)
    in parens (text "step-with-rollback" <> space <> formatExpr env expr <> space <> formatExpr env rollback <> annsDoc)

formatIfDef :: FormatEnv -> IfDef () -> Doc
formatIfDef env = \case
  IfDfun d -> formatIfDefun env d
  IfDConst d -> formatDefConst env d
  IfDCap d -> formatIfDefCap env d
  IfDSchema d -> formatDefSchema env d
  IfDPact d -> formatIfDefPact env d

formatIfDefun :: FormatEnv -> IfDefun () -> Doc
formatIfDefun _ (IfDefun spec args anns _) =
  let headerDoc = text "defun" <> space <> formatMArg spec <> space <> formatArgs args
      annsDoc = if null anns then mempty else space <> vcat (map formatPactAnn anns)
  in parens (headerDoc <> annsDoc)

formatIfDefCap :: FormatEnv -> IfDefCap () -> Doc
formatIfDefCap _ (IfDefCap spec args anns mmeta _) =
  let headerDoc = text "defcap" <> space <> formatMArg spec <> space <> formatArgs args
      annsDoc = map formatPactAnn anns
      metaDoc = case mmeta of
        Nothing -> []
        Just meta -> [formatDCapMeta meta]
      allAnns = annsDoc ++ metaDoc
      annsDocFinal = if null allAnns then mempty else space <> vcat allAnns
  in parens (headerDoc <> annsDocFinal)

formatIfDefPact :: FormatEnv -> IfDefPact () -> Doc
formatIfDefPact _ (IfDefPact spec args anns _) =
  let headerDoc = text "defpact" <> space <> formatMArg spec <> space <> formatArgs args
      annsDoc = if null anns then mempty else space <> vcat (map formatPactAnn anns)
  in parens (headerDoc <> annsDoc)

formatArgs :: [MArg ()] -> Doc
formatArgs args = parens (hsep $ map formatMArg args)

formatArg :: PT.Arg () -> Doc
formatArg (PT.Arg name ty _) = text name <> char ':' <> formatType ty

formatMArg :: MArg () -> Doc
formatMArg (MArg name mty _) =
  case mty of
    Nothing -> text name
    Just ty -> text name <> char ':' <> formatType ty

formatType :: Type -> Doc
formatType = \case
  TyPrim prim -> formatPrimType prim
  TyList ty -> brackets (formatType ty)
  TyPolyList -> text "list"
  TyModRef mns -> text "module" <> braces (hsep $ punctuate comma $ map formatModuleName mns)
  TyKeyset -> text "keyset"
  TyObject qn -> text "object" <> braces (formatParsedTyName qn)
  TyPolyObject -> text "object"
  TyTable qn -> text "table" <> braces (formatParsedTyName qn)
  TyAny -> text "*"

formatPrimType :: PrimType -> Doc
formatPrimType = \case
  PrimInt -> text "integer"
  PrimDecimal -> text "decimal"
  PrimBool -> text "bool"
  PrimString -> text "string"
  PrimTime -> text "time"
  PrimUnit -> text "unit"
  PrimGuard -> text "guard"

formatBody :: FormatEnv -> NonEmpty (Expr ()) -> Doc
formatBody env exprs = vcat $ map (formatExpr env) (NE.toList exprs)

formatExpr :: FormatEnv -> Expr () -> Doc
formatExpr env = \case
  Var pn _ -> formatParsedName pn
  Let lf bindings body _ -> formatLet env lf bindings body
  Lam args body _ -> formatLambda env args body
  App fn args _ -> formatApp env fn args
  List items _ -> formatList env items
  Constant lit _ -> formatLiteral lit
  Object pairs _ -> formatObject env pairs
  Binding pairs body _ -> formatBinding env pairs body

formatLet :: FormatEnv -> LetForm -> NonEmpty (Binder ()) -> NonEmpty (Expr ()) -> Doc
formatLet env lf bindings body =
  let indent' = cfgIndentSize (fmtConfig env)
      letKw = case lf of
        LFLetNormal -> text "let"
        LFLetStar -> text "let*"
      bindingsDoc = if cfgAlignLetBindings (fmtConfig env)
                    then formatAlignedBindings env (NE.toList bindings)
                    else parens (vsep $ map (formatBinder env) (NE.toList bindings))
      bodyDoc = vsep $ map (formatExpr env) (NE.toList body)
  in parens (letKw <> space <> bindingsDoc <> nest indent' (softline <> bodyDoc))

formatBinder :: FormatEnv -> Binder () -> Doc
formatBinder env (Binder marg expr) =
  parens (formatMArg marg <> space <> formatExpr env expr)

formatAlignedBindings :: FormatEnv -> [Binder ()] -> Doc
formatAlignedBindings env bindings =
  let formatted = map (\(Binder marg expr) -> (formatMArg marg, formatExpr env expr)) bindings
      maxLen = maximum (0 : map (T.length . renderDoc 999 . fst) formatted)
      aligned = map (\(name, expr) -> 
        let nameText = renderDoc 999 name
            padding = T.replicate (fromIntegral maxLen - T.length nameText + 1) " "
        in parens (name <> text padding <> expr)) formatted
  in parens (align (vcat aligned))

formatLambda :: FormatEnv -> [MArg ()] -> NonEmpty (Expr ()) -> Doc
formatLambda env args body =
  let indent' = cfgIndentSize (fmtConfig env)
      argsDoc = formatArgs args
      bodyDoc = vsep $ map (formatExpr env) (NE.toList body)
  in parens (text "lambda" <> space <> argsDoc <> nest indent' (line <> bodyDoc))

formatApp :: FormatEnv -> Expr () -> [Expr ()] -> Doc
formatApp env fn args =
  let fnDoc = formatExpr env fn
      argsDoc = map (formatExpr env) args
      totalDoc = fnDoc : argsDoc
      simpleApp = parens (hsep totalDoc)
      breakLong = cfgBreakLongFunctionCalls (fmtConfig env)
      -- Check if we need special formatting for certain functions
      specialFormat = case fn of
        Var (BN (BareName name)) _ | name `elem` specialForms -> True
        _ -> False
      -- Estimate length
      estimatedLen = sum (map (T.length . renderDoc 999) totalDoc) + length totalDoc + 2
  in if specialFormat
     then formatSpecialForm env fn args
     else if breakLong && estimatedLen > cfgMaxLineWidth (fmtConfig env) `div` 2
          then parens (fnDoc <> nest (cfgIndentSize (fmtConfig env)) (line <> vsep argsDoc))
          else simpleApp
  where
    specialForms = ["if", "and", "or", "enforce", "enforce-one", "with-capability", "create-user-guard"]

formatSpecialForm :: FormatEnv -> Expr () -> [Expr ()] -> Doc
formatSpecialForm env fn args =
  let indent' = cfgIndentSize (fmtConfig env)
      fnDoc = formatExpr env fn
  in case (fn, args) of
    (Var (BN (BareName "if")) _, [cond, thenExpr, elseExpr]) ->
      parens $ fnDoc <> space <> formatExpr env cond <>
        nest indent' (line <> formatExpr env thenExpr <>
                      line <> formatExpr env elseExpr)
    (Var (BN (BareName "with-capability")) _, [cap, body]) ->
      parens $ fnDoc <> space <> formatExpr env cap <>
        nest indent' (line <> formatExpr env body)
    (Var (BN (BareName "enforce")) _, [cond, msg]) ->
      parens $ fnDoc <> space <> formatExpr env cond <> space <> formatExpr env msg
    (Var (BN (BareName "enforce-one")) _, [msg, conds]) ->
      parens $ fnDoc <> space <> formatExpr env msg <>
        nest indent' (line <> formatExpr env conds)
    _ -> parens (hsep $ fnDoc : map (formatExpr env) args)

formatList :: FormatEnv -> [Expr ()] -> Doc
formatList env items =
  let itemDocs = map (formatExpr env) items
  in brackets (hsep $ punctuate comma itemDocs)

formatObject :: FormatEnv -> [(Field, Expr ())] -> Doc
formatObject env pairs =
  let pairDocs = map (\(Field f, e) -> dquotes (text f) <> char ':' <> formatExpr env e) pairs
  in braces (hsep $ punctuate comma pairDocs)

formatBinding :: FormatEnv -> [(Field, MArg ())] -> [Expr ()] -> Doc
formatBinding env binds body =
  let bindDocs = map (\(Field f, marg) -> text f <> text ":=" <> formatMArg marg) binds
      bindsDoc = braces (hsep $ punctuate comma bindDocs)
      bodyDoc = hsep $ map (formatExpr env) body
  in bindsDoc <> space <> bodyDoc

formatLiteral :: Literal -> Doc
formatLiteral = \case
  LString s -> dquotes (text s)
  LInteger i -> text (T.pack $ show i)
  LDecimal d -> text (T.pack $ show d)
  LBool b -> text (if b then "true" else "false")
  LUnit -> text "()"

formatParsedName :: ParsedName -> Doc
formatParsedName = \case
  BN (BareName n) -> text n
  QN (QualifiedName n mn) -> formatModuleName mn <> char '.' <> text n
  DN (DynamicName n call) -> text n <> text "::" <> text call

formatModuleName :: ModuleName -> Doc
formatModuleName (ModuleName mn mns) = 
  case mns of
    Nothing -> text mn
    Just (NamespaceName ns) -> text ns <> char '.' <> text mn

formatParsedTyName :: ParsedTyName -> Doc
formatParsedTyName = \case
  TBN (BareName n) -> text n
  TQN (QualifiedName n mn) -> formatModuleName mn <> char '.' <> text n