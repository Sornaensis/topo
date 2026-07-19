module Spec.Documentation (spec) where

import Control.Monad (filterM, forM, forM_)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (isPrefixOf, sort)
import qualified Data.Set as Set
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  )
import System.FilePath
  ( isAbsolute
  , makeRelative
  , normalise
  , takeDirectory
  , takeExtension
  , (</>)
  )
import Test.Hspec

spec :: Spec
spec = describe "Documentation" $ do
  it "parses reference links and stable Markdown heading anchors" $ do
    let referenceDocument = unlines
          [ "Read the [guide][docs]."
          , ""
          , "[docs]: docs/README.md#sources-of-truth"
          ]
    fmap (map mlTarget) (markdownLinks referenceDocument)
      `shouldBe` Right ["docs/README.md#sources-of-truth"]
    markdownLinks "Read the [guide][missing]."
      `shouldSatisfy` isLeft
    markdownAnchors (unlines ["# Foo", "# Foo-1", "# Foo", "Setext", "======"])
      `shouldBe` Set.fromList ["foo", "foo-1", "foo-2", "setext"]

  it "rejects retired paths, numeric coverage claims, and CI claims" $ do
    forbiddenReferenceReasons "see release-notes/1.0.md"
      `shouldSatisfy` (not . null)
    forM_
      [ "see docs\\release-notes\\1.0.md"
      , "see docs\\api\\README.md"
      , "see plugin-dev\\README.md"
      ] $ \windowsPath ->
        forbiddenReferenceReasons windowsPath
          `shouldSatisfy` (not . null)
    forbiddenReferenceReasons "coverage must remain above 80"
      `shouldSatisfy` (not . null)
    forbiddenReferenceReasons "ci enforces the test suite"
      `shouldSatisfy` (not . null)
    forbiddenReferenceReasons "use operator/openapi.json"
      `shouldBe` []

  it "contains exactly the approved audience documentation tree" $
    withRepositoryRoot $ \root -> do
      actual <- sort . map (toRepositoryPath . makeRelative root)
        <$> listFilesRecursively (root </> "docs")
      actual `shouldBe` approvedDocumentationPaths

  it "resolves every local Markdown target and anchor" $
    withRepositoryRoot $ \root -> do
      docsFiles <- listFilesRecursively (root </> "docs")
      let markdownFiles =
            [ root </> "README.md"
            , root </> "CHANGELOG.md"
            , root </> "topo-seer" </> "README.md"
            ] <> filter ((== ".md") . map toLower . takeExtension) docsFiles
      problems <- concat <$> mapM (markdownLinkProblems root) markdownFiles
      problems `shouldBe` []

  it "does not resurrect retired documentation or repository references" $
    withRepositoryRoot $ \root -> do
      docsFiles <- listFilesRecursively (root </> "docs")
      let markdownFiles =
            [ root </> "README.md"
            , root </> "CHANGELOG.md"
            , root </> "topo-seer" </> "README.md"
            ] <> filter ((== ".md") . map toLower . takeExtension) docsFiles
      problems <- concat <$> mapM (forbiddenReferenceProblems root) markdownFiles
      problems `shouldBe` []

approvedDocumentationPaths :: [FilePath]
approvedDocumentationPaths = sort
  [ "docs/README.md"
  , "docs/user/getting-started.md"
  , "docs/user/topo-seer.md"
  , "docs/operator/README.md"
  , "docs/operator/openapi.json"
  , "docs/integrator/README.md"
  , "docs/integrator/file-formats.md"
  , "docs/plugin/README.md"
  , "docs/plugin/protocol.md"
  , "docs/plugin/manifest-v3.schema.json"
  , "docs/plugin/examples/provider.json"
  , "docs/plugin/examples/consumer.json"
  , "docs/contributor/README.md"
  , "docs/migration/1.0.md"
  ]

withRepositoryRoot :: (FilePath -> IO ()) -> IO ()
withRepositoryRoot action = do
  mRoot <- repositoryRoot
  case mRoot of
    Just root -> action root
    Nothing -> pendingWith
      "repository documentation is unavailable in this package-only test run"

repositoryRoot :: IO (Maybe FilePath)
repositoryRoot = do
  cwd <- getCurrentDirectory
  -- Only inspect the test working directory and its direct parent. In an sdist
  -- run this avoids escaping .stack-work and reading an outer checkout.
  roots <- filterM isRepositoryRoot [cwd, takeDirectory cwd]
  pure $ case roots of
    root:_ -> Just root
    [] -> Nothing
  where
    isRepositoryRoot path = and <$> sequence
      [ doesFileExist (path </> "stack.yaml")
      , doesFileExist (path </> "topo" </> "package.yaml")
      , doesFileExist (path </> "topo-seer" </> "package.yaml")
      ]

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively directory = do
  entries <- listDirectory directory
  fmap concat $ forM entries $ \entry -> do
    let path = directory </> entry
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then listFilesRecursively path
      else do
        isFile <- doesFileExist path
        pure [path | isFile]

markdownLinkProblems :: FilePath -> FilePath -> IO [String]
markdownLinkProblems root source = do
  contents <- readFile source
  case markdownLinks contents of
    Left problem -> pure [displaySource root source <> ": " <> problem]
    Right links -> concat <$> mapM (validateMarkdownLink root source) links

validateMarkdownLink :: FilePath -> FilePath -> MarkdownLink -> IO [String]
validateMarkdownLink root source markdownLink =
  case parseLinkTarget (mlTarget markdownLink) of
    Left problem -> pure [prefix <> problem]
    Right Nothing -> pure []
    Right (Just (targetPath, fragment)) -> do
      let resolved = normalise $ if null targetPath
            then source
            else takeDirectory source </> targetPath
          relative = makeRelative root resolved
      if escapesRepository relative
        then pure [prefix <> "target escapes the repository: " <> mlTarget markdownLink]
        else do
          fileExists <- doesFileExist resolved
          directoryExists <- doesDirectoryExist resolved
          if not (fileExists || directoryExists)
            then pure [prefix <> "missing local target: " <> mlTarget markdownLink]
            else validateFragment prefix resolved fileExists fragment
  where
    prefix = displaySource root source <> ":" <> show (mlLine markdownLink) <> ": "

validateFragment :: String -> FilePath -> Bool -> String -> IO [String]
validateFragment _ _ _ "" = pure []
validateFragment prefix resolved fileExists fragment
  | not fileExists = pure [prefix <> "anchor target is not a file: #" <> fragment]
  | map toLower (takeExtension resolved) /= ".md" =
      pure [prefix <> "anchor target is not Markdown: #" <> fragment]
  | otherwise = do
      anchors <- markdownAnchors <$> readFile resolved
      pure
        [ prefix <> "missing local anchor: #" <> fragment
        | map toLower fragment `Set.notMember` anchors
        ]

parseLinkTarget :: String -> Either String (Maybe (FilePath, String))
parseLinkTarget raw = do
  target <- markdownDestination raw
  if hasUriScheme target || "//" `isPrefixOf` target
    then Right Nothing
    else do
      if any isSpace target
        then Left ("unescaped whitespace in local target: " <> raw)
        else Right ()
      if '\\' `elem` target
        then Left ("local target must use '/' separators: " <> raw)
        else Right ()
      let (pathAndQuery, fragmentWithHash) = break (== '#') target
          targetPath = takeWhile (/= '?') pathAndQuery
          fragment = case fragmentWithHash of
            '#':value -> value
            _ -> ""
      if null targetPath && null fragment
        then Left "empty local link target"
        else Right ()
      if isAbsolute targetPath || "/" `isPrefixOf` targetPath
        then Left ("local target must be repository-relative: " <> raw)
        else Right ()
      Right (Just (targetPath, fragment))

hasUriScheme :: String -> Bool
hasUriScheme target = case break (== ':') target of
  (first:rest, ':':_)
    | isAlpha first -> all isSchemeCharacter rest
  _ -> False
  where
    isSchemeCharacter char = isAlphaNum char || char `elem` "+-."

markdownDestination :: String -> Either String String
markdownDestination raw =
  case trim raw of
    [] -> Left "empty Markdown link destination"
    '<':rest -> case break (== '>') rest of
      (_, []) -> Left ("unterminated angle-bracket link target: " <> raw)
      (target, _:suffix) -> validateTitle raw target suffix
    value ->
      let (target, suffix) = break isSpace value
      in validateTitle raw target suffix

validateTitle :: String -> String -> String -> Either String String
validateTitle raw target suffix
  | null target = Left ("empty Markdown link destination: " <> raw)
  | null title = Right target
  | isQuoted '"' title || isQuoted '\'' title || isQuotedPair '(' ')' title = Right target
  | otherwise = Left ("malformed Markdown link destination or title: " <> raw)
  where
    title = trim suffix
    isQuoted quote = isQuotedPair quote quote
    isQuotedPair open close (first:remaining) =
      first == open && case reverse remaining of
        final:_ -> final == close
        [] -> False
    isQuotedPair _ _ [] = False

data MarkdownLink = MarkdownLink
  { mlLine :: Int
  , mlTarget :: String
  } deriving (Show)

markdownLinks :: String -> Either String [MarkdownLink]
markdownLinks contents = do
  let prose = markdownProseLines contents
  inlineLinks <- fmap concat . sequence $
    [ extractLineLinks lineNumber line
    | (lineNumber, line) <- prose
    ]
  definitions <- fmap concat . sequence $
    [ extractReferenceDefinition lineNumber line
    | (lineNumber, line) <- prose
    ]
  let definitionMap = foldl insertDefinition (Right []) definitions
  knownDefinitions <- definitionMap
  usages <- fmap concat . sequence $
    [ extractReferenceUsages lineNumber line
    | (lineNumber, line) <- prose
    ]
  forM_ usages $ \(lineNumber, label) ->
    if normalizeReferenceLabel label `elem` map fst knownDefinitions
      then Right ()
      else Left ("undefined Markdown reference [" <> label <> "] near line " <> show lineNumber)
  pure (inlineLinks <> map snd knownDefinitions)
  where
    insertDefinition (Left problem) _ = Left problem
    insertDefinition (Right known) definition@(label, link)
      | label `elem` map fst known =
          Left ("duplicate Markdown reference [" <> label <> "] near line " <> show (mlLine link))
      | otherwise = Right (known <> [definition])

extractReferenceDefinition :: Int -> String -> Either String [(String, MarkdownLink)]
extractReferenceDefinition lineNumber line =
  case dropWhile isSpace line of
    '[':rest -> case break (== ']') rest of
      (label, ']':':':target)
        | not (null (trim label)) ->
            Right [(normalizeReferenceLabel label, MarkdownLink lineNumber (trim target))]
      _ -> Right []
    _ -> Right []

extractReferenceUsages :: Int -> String -> Either String [(Int, String)]
extractReferenceUsages lineNumber = go
  where
    go [] = Right []
    go ('[':rest) = case break (== ']') rest of
      (firstLabel, ']':'[':afterOpen) -> case break (== ']') afterOpen of
        (_, []) -> Left ("unterminated Markdown reference near line " <> show lineNumber)
        (secondLabel, _:remaining) ->
          let label = if null secondLabel then firstLabel else secondLabel
          in ((lineNumber, label) :) <$> go remaining
      _ -> go rest
    go (_:rest) = go rest

normalizeReferenceLabel :: String -> String
normalizeReferenceLabel = unwords . words . map toLower

extractLineLinks :: Int -> String -> Either String [MarkdownLink]
extractLineLinks lineNumber = go
  where
    go input = case breakOn "]( " input of
      Just _ -> Left ("malformed Markdown link near line " <> show lineNumber)
      Nothing -> case breakOn "](" input of
        Nothing -> Right []
        Just (_, afterOpen) -> case takeDestination 0 [] afterOpen of
          Left problem -> Left (problem <> " near line " <> show lineNumber)
          Right (target, rest) ->
            (MarkdownLink lineNumber target :) <$> go rest

    takeDestination _ _ [] = Left "unterminated Markdown link"
    takeDestination depth acc (char:rest)
      | char == ')' && depth == 0 = Right (reverse acc, rest)
      | char == ')' = takeDestination (depth - 1) (char:acc) rest
      | char == '(' = takeDestination (depth + 1) (char:acc) rest
      | otherwise = takeDestination depth (char:acc) rest

breakOn :: String -> String -> Maybe (String, String)
breakOn needle = search []
  where
    search _ [] = Nothing
    search prefix remaining@(char:rest)
      | needle `isPrefixOf` remaining =
          Just (reverse prefix, drop (length needle) remaining)
      | otherwise = search (char : prefix) rest

markdownProseLines :: String -> [(Int, String)]
markdownProseLines = go False . zip [1..] . lines
  where
    go _ [] = []
    go inFence ((lineNumber, line):rest)
      | isFence line = go (not inFence) rest
      | inFence = go True rest
      | otherwise = (lineNumber, line) : go False rest
    isFence line =
      let stripped = dropWhile isSpace line
      in "```" `isPrefixOf` stripped || "~~~" `isPrefixOf` stripped

markdownAnchors :: String -> Set.Set String
markdownAnchors contents = foldl addHeading Set.empty headings
  where
    prose = markdownProseLines contents
    headings = map snd . sort $
      [ (lineNumber, heading)
      | (lineNumber, line) <- prose
      , Just heading <- [headingText line]
      ] <> setextHeadings prose
    addHeading used heading = Set.insert (uniqueAnchor used (githubSlug heading)) used

uniqueAnchor :: Set.Set String -> String -> String
uniqueAnchor used base
  | base `Set.notMember` used = base
  | otherwise = firstAvailable 1
  where
    firstAvailable suffix =
      let candidate = base <> "-" <> show suffix
      in if candidate `Set.member` used
          then firstAvailable (suffix + 1)
          else candidate

setextHeadings :: [(Int, String)] -> [(Int, String)]
setextHeadings ((lineNumber, title):underlined@((_, underline):rest))
  | isSetextUnderline underline = (lineNumber, trim title) : setextHeadings underlined
  | otherwise = setextHeadings underlined
setextHeadings _ = []

isSetextUnderline :: String -> Bool
isSetextUnderline line = case trim line of
  marker:markers -> marker `elem` "=-" && not (null markers) && all (== marker) markers
  [] -> False

headingText :: String -> Maybe String
headingText line =
  let stripped = dropWhile isSpace line
      (hashes, rest) = span (== '#') stripped
  in case rest of
      first:_
        | not (null hashes) && length hashes <= 6 && isSpace first ->
            Just (dropTrailingHashes (trim rest))
      _ -> Nothing

githubSlug :: String -> String
githubSlug = trimHyphens . concatMap slugCharacter . map toLower
  where
    slugCharacter char
      | isAlphaNum char || char == '-' || char == '_' = [char]
      | isSpace char = "-"
      | otherwise = ""
    trimHyphens = reverse . dropWhile (== '-') . reverse . dropWhile (== '-')

dropTrailingHashes :: String -> String
dropTrailingHashes = reverse . dropWhile isSpace . dropWhile (== '#') . dropWhile isSpace . reverse

forbiddenReferenceProblems :: FilePath -> FilePath -> IO [String]
forbiddenReferenceProblems root source = do
  lowered <- map toLower <$> readFile source
  let prefix = displaySource root source <> ": forbidden documentation reference: "
  pure [prefix <> problem | problem <- forbiddenReferenceReasons lowered]

forbiddenReferenceReasons :: String -> [String]
forbiddenReferenceReasons input = Set.toList . Set.fromList $
  [ patternText
  | patternText <- forbiddenReferencePatterns
  , patternText `isInfixOfString` normalized
  ] <>
  [ "retired documentation path: " <> pathToken
  | pathToken <- retiredPathTokens
  , containsPathToken pathToken normalized
  ] <>
  [ "numeric coverage threshold or claim"
  | line <- lines normalized
  , "coverage" `isInfixOfString` line
  , any isDigit line || any (`isInfixOfString` line) ["%", "percent", "threshold"]
  ] <>
  [ "ci claim"
  | line <- lines normalized
  , "ci" `elem` documentationWords line
  ]
  where
    normalized = map normalizePathSeparator input
    normalizePathSeparator '\\' = '/'
    normalizePathSeparator char = char

retiredPathTokens :: [String]
retiredPathTokens =
  [ "api/"
  , "plugin-dev/"
  , "inventory/"
  , "release-notes/"
  , "roadmap/"
  , "spec/"
  , "specs/"
  , "internal/"
  , "release.md"
  , "pre-1.0-to-1.0.md"
  ]

containsPathToken :: String -> String -> Bool
containsPathToken token = go Nothing
  where
    go _ [] = False
    go previous remaining@(char:rest)
      | token `isPrefixOf` remaining && maybe True (not . isPathNameCharacter) previous = True
      | otherwise = go (Just char) rest
    isPathNameCharacter char = isAlphaNum char || char `elem` "-_."

documentationWords :: String -> [String]
documentationWords = words . map (\char -> if isAlphaNum char then char else ' ')

forbiddenReferencePatterns :: [String]
forbiddenReferencePatterns =
  [ ".github/"
  , "tools/"
  , "docs/inventory"
  , "public-surface.json"
  , "public-surface.md"
  , "spec.featurematrix"
  , "feature-matrix"
  , "feature matrix"
  , "docs/api"
  , "docs/plugin-dev"
  , "docs/release"
  , "docs/roadmap"
  , "docs/spec"
  , "docs/internal"
  , "docs/migration/pre-1.0"
  , "mcp"
  , "command ipc"
  , "coverage.py"
  , "check_coverage"
  , "check-coverage-thresholds.py"
  , "ci gate"
  , "ci workflow"
  , "ci pipeline"
  , "ci artifact"
  , "ci job"
  , "ci check"
  , "github actions"
  , "continuous integration"
  , "automation or ci"
  , "ci smoke"
  ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isInfixOfString :: String -> String -> Bool
isInfixOfString needle haystack = any (needle `isPrefixOf`) (tailsOf haystack)

tailsOf :: [a] -> [[a]]
tailsOf [] = [[]]
tailsOf value@(_:rest) = value : tailsOf rest

escapesRepository :: FilePath -> Bool
escapesRepository relative =
  relative == ".."
    || "../" `isPrefixOf` toRepositoryPath relative

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd predicate = reverse . dropWhile predicate . reverse

displaySource :: FilePath -> FilePath -> FilePath
displaySource root = toRepositoryPath . makeRelative root

toRepositoryPath :: FilePath -> FilePath
toRepositoryPath = map $ \char -> if char == '\\' then '/' else char
