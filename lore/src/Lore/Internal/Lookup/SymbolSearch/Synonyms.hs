module Lore.Internal.Lookup.SymbolSearch.Synonyms
  ( builtInSynonymGroups,
    SynonymTerm (..),
    SynonymLexicon (..),
    SynonymTermError (..),
    SynonymGroupError (..),
    builtInSynonymLexicon,
    compileSynonymTerm,
    compileSynonymGroups,
    mergeSynonymLexicons,
    directSynonyms,
    renderInvalidBuiltInSynonymGroups,
    renderSynonymGroupError,
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Lookup.SymbolSearch.Tokenize (canonicalizeSearchToken, tokenizeSearchText)
import Lore.Internal.Lookup.SymbolSearch.Types (SearchToken (..), SynonymTerm (..))

newtype SynonymLexicon = SynonymLexicon
  { unSynonymLexicon :: Map.Map SynonymTerm (Set.Set SynonymTerm)
  }
  deriving stock (Eq, Show)

instance Semigroup SynonymLexicon where
  SynonymLexicon left <> SynonymLexicon right =
    SynonymLexicon (Map.unionWith Set.union left right)

instance Monoid SynonymLexicon where
  mempty =
    SynonymLexicon Map.empty

data SynonymGroupError
  = SynonymGroupHasTooFewTerms Int
  | SynonymTermProducesNoToken Int Int Text
  | SynonymGroupHasTooFewDistinctTerms Int [SynonymTerm]
  deriving stock (Eq, Show)

data SynonymTermError
  = SynonymTermProducesNoTokens Text
  deriving stock (Eq, Show)

builtInSynonymGroups :: [[Text]]
builtInSynonymGroups =
  [ -- Storage / databases
    ["db", "database"],
    ["rdbms", "sql", "relational"],
    ["nosql", "document"],
    ["kv", "key value"],
    ["cache", "memo", "memoize"],
    ["redis", "cache"],
    ["postgres", "postgresql", "pg"],
    ["mysql", "mariadb"],
    ["mongo", "mongodb"],
    ["elastic", "elasticsearch", "opensearch"],
    ["index", "idx"],
    ["schema", "structure"],
    ["migration", "migrate"],
    ["seed", "fixture"],
    ["repo", "repository", "dao"],
    ["orm", "mapper"],
    ["entity", "model", "record"],
    ["table", "relation"],
    ["column", "field", "attribute"],
    ["row", "tuple", "record"],
    ["query", "select", "lookup"],
    ["insert", "create", "add"],
    ["update", "modify", "edit", "patch"],
    ["delete", "remove", "destroy"],
    ["truncate", "purge", "clear"],
    ["transaction", "tx"],
    ["commit", "save"],
    ["rollback", "revert"],
    ["lock", "mutex", "semaphore"],
    ["deadlock", "starvation"],
    ["replica", "replication"],
    ["shard", "partition"],
    ["primary", "master"],
    ["secondary", "replica", "slave"],
    ["backup", "dump", "snapshot"],
    ["restore", "recover"],
    ["persist", "store", "save"],
    ["hydrate", "populate"],
    ["serialize", "encode", "marshal"],
    ["deserialize", "decode", "unmarshal"],
    -- Auth / identity / security
    ["auth", "authentication"],
    ["authz", "authorization"],
    ["signin", "login"],
    ["signout", "logout"],
    ["signup", "register"],
    ["user", "account", "principal"],
    ["credential", "secret"],
    ["password", "passwd", "pwd"],
    ["token", "jwt"],
    ["session", "cookie"],
    ["permission", "privilege", "grant"],
    ["role", "scope", "claim"],
    ["acl", "rbac"],
    ["oauth", "openid", "oidc"],
    ["sso", "federation"],
    ["mfa", "2fa", "totp"],
    ["hash", "digest"],
    ["salt", "nonce"],
    ["encrypt", "cipher"],
    ["decrypt", "decipher"],
    ["crypto", "cryptography"],
    ["certificate", "cert"],
    ["tls", "ssl", "https"],
    ["csrf", "xsrf"],
    ["xss", "injection"],
    ["sanitize", "escape"],
    ["validate", "verify", "check"],
    ["captcha", "recaptcha"],
    -- HTTP / API / networking
    ["api", "endpoint"],
    ["rest", "restful"],
    ["graphql", "gql"],
    ["rpc", "grpc", "json rpc", "xml rpc"],
    ["request", "req"],
    ["response", "res", "resp"],
    ["header", "hdr"],
    ["body", "payload"],
    ["url", "uri", "href"],
    ["route", "path"],
    ["router", "routing"],
    ["middleware", "interceptor", "filter"],
    ["handler", "controller", "action"],
    ["client", "consumer"],
    ["server", "backend", "service"],
    ["proxy", "gateway"],
    ["reverse proxy", "gateway"],
    ["webhook", "callback"],
    ["poll", "polling"],
    ["retry", "reattempt"],
    ["timeout", "deadline"],
    ["latency", "delay"],
    ["throughput", "bandwidth"],
    ["rate", "quota", "limit"],
    ["rate limit", "throttle"],
    ["status", "code"],
    ["redirect", "forward"],
    ["upload", "send"],
    ["download", "receive"],
    ["socket", "sock"],
    ["websocket", "ws"],
    ["tcp", "stream"],
    ["udp", "datagram"],
    ["dns", "resolver"],
    ["ip", "address"],
    ["port", "listener"],
    -- CRUD / data access verbs
    ["get", "load", "fetch", "retrieve", "read"],
    ["find", "search", "lookup", "query"],
    ["list", "enumerate", "scan"],
    ["create", "new", "make", "build"],
    ["add", "append", "insert"],
    ["set", "assign", "put"],
    ["change", "update", "modify", "edit"],
    ["delete", "remove", "drop", "destroy"],
    ["copy", "clone", "duplicate"],
    ["move", "rename", "relocate"],
    ["merge", "combine", "join"],
    ["split", "divide", "partition"],
    ["parse", "read", "decode"],
    ["format", "render", "print"],
    ["normalize", "canonicalize"],
    ["convert", "transform", "map"],
    ["filter", "select"],
    ["reduce", "fold", "aggregate"],
    ["sort", "order"],
    ["compare", "cmp"],
    ["equal", "eq"],
    ["match", "test"],
    ["contains", "include", "has"],
    ["init", "initialize", "setup"],
    ["cleanup", "teardown", "dispose"],
    ["start", "run", "launch"],
    ["stop", "halt", "shutdown"],
    ["restart", "reload"],
    ["enable", "activate"],
    ["disable", "deactivate"],
    -- Code structure
    ["fn", "func", "function"],
    ["method", "member"],
    ["proc", "procedure", "routine"],
    ["var", "variable"],
    ["const", "constant"],
    ["param", "parameter", "arg", "argument"],
    ["prop", "property"],
    ["attr", "attribute"],
    ["field", "member"],
    ["obj", "object"],
    ["klass", "class", "type"],
    ["iface", "interface", "protocol", "trait"],
    ["impl", "implementation"],
    ["mod", "module"],
    ["pkg", "package"],
    ["lib", "library"],
    ["dep", "dependency"],
    ["import", "include", "require"],
    ["export", "expose"],
    ["namespace", "ns"],
    ["enum", "enumeration"],
    ["struct", "record"],
    ["union", "variant"],
    ["alias", "typedef"],
    ["generic", "template"],
    ["annotation", "decorator", "attribute"],
    ["metadata", "meta"],
    ["comment", "doc"],
    ["todo", "fixme", "hack"],
    ["deprecate", "obsolete", "legacy"],
    -- Types / values / data shapes
    ["bool", "boolean"],
    ["int", "integer"],
    ["float", "double", "decimal"],
    ["str", "string", "text"],
    ["char", "character"],
    ["num", "number", "numeric"],
    ["arr", "array", "list"],
    ["vec", "vector"],
    ["dict", "map", "hashmap"],
    ["set", "collection"],
    ["tuple", "pair"],
    ["option", "optional", "maybe", "nullable"],
    ["nullable", "nil", "null"],
    ["none", "nil", "null"],
    ["uuid", "guid"],
    ["id", "identifier"],
    ["ref", "reference"],
    ["ptr", "pointer"],
    ["buf", "buffer"],
    ["blob", "binary"],
    ["json", "object"],
    ["yaml", "yml"],
    ["xml", "markup"],
    ["csv", "tsv"],
    ["html", "markup"],
    ["css", "style"],
    ["regex", "regexp", "pattern"],
    -- Errors / logging / observability
    ["err", "error", "exception", "failure"],
    ["warn", "warning"],
    ["info", "notice"],
    ["debug", "trace"],
    ["fatal", "panic", "critical"],
    ["log", "logger", "logging"],
    ["metric", "measurement"],
    ["monitor", "observe"],
    ["telemetry", "observability"],
    ["trace", "span"],
    ["profile", "profiling"],
    ["benchmark", "bench"],
    ["diagnostic", "diag"],
    ["health", "heartbeat"],
    ["alert", "alarm"],
    ["incident", "outage"],
    ["stacktrace", "backtrace"],
    ["cause", "reason"],
    ["fallback", "default"],
    ["recover", "rescue", "handle"],
    ["raise", "throw"],
    ["catch", "except"],
    ["assert", "ensure"],
    ["invariant", "constraint"],
    -- Testing / quality
    ["test", "spec", "check"],
    ["unit", "component"],
    ["integration", "e2e"],
    ["mock", "stub", "fake", "spy"],
    ["fixture", "sample"],
    ["factory", "builder"],
    ["snapshot", "golden"],
    ["coverage", "cov"],
    ["lint", "analyze"],
    ["format", "fmt", "prettify"],
    ["qa", "quality"],
    ["bug", "defect", "issue"],
    ["regression", "breakage"],
    ["flake", "flaky"],
    ["expected", "want"],
    ["actual", "got"],
    ["pass", "success"],
    ["fail", "failure"],
    ["skip", "ignore"],
    ["case", "scenario"],
    ["suite", "collection"],
    -- Build / CI / deployment
    ["ci", "cicd"],
    ["cd", "deploy"],
    ["build", "compile"],
    ["compiler", "transpiler"],
    ["bundle", "pack"],
    ["minify", "compress"],
    ["artifact", "output"],
    ["release", "version"],
    ["tag", "label"],
    ["branch", "ref"],
    ["commit", "revision"],
    ["merge", "rebase"],
    ["pr", "pull request"],
    ["mr", "merge request"],
    ["repo", "repository"],
    ["vcs", "git"],
    ["checkout", "switch"],
    ["stash", "shelve"],
    ["diff", "patch"],
    ["changelog", "history"],
    ["semver", "versioning"],
    ["major", "breaking"],
    ["minor", "feature"],
    ["patch", "fix"],
    ["deploy", "publish", "ship"],
    ["rollback", "revert"],
    ["pipeline", "workflow"],
    ["job", "task", "step"],
    ["runner", "agent", "worker"],
    ["env", "environment"],
    ["stage", "phase"],
    ["prod", "production"],
    ["staging", "preprod"],
    ["dev", "development"],
    ["local", "localhost"],
    ["config", "cfg", "configuration"],
    ["setting", "option", "preference"],
    -- Infrastructure / cloud / containers
    ["infra", "infrastructure"],
    ["ops", "operation"],
    ["sre", "devops"],
    ["cloud", "provider"],
    ["aws", "amazon"],
    ["gcp", "google"],
    ["azure", "microsoft"],
    ["vm", "machine", "instance"],
    ["container", "docker"],
    ["image", "container image"],
    ["k8s", "kubernetes"],
    ["pod", "container"],
    ["node", "host"],
    ["cluster", "fleet"],
    ["service", "svc"],
    ["ingress", "gateway"],
    ["lb", "load balancer"],
    ["cdn", "edge"],
    ["region", "zone"],
    ["secret", "credential"],
    ["vault", "keystore"],
    ["volume", "disk", "storage"],
    ["mount", "attach"],
    ["cpu", "processor"],
    ["mem", "memory", "ram"],
    ["gpu", "accelerator"],
    ["auto scale", "scale"],
    ["provision", "allocate"],
    ["terraform", "tf"],
    ["ansible", "playbook"],
    ["helm", "chart"],
    -- Async / concurrency / scheduling
    ["async", "asynchronous"],
    ["sync", "synchronous"],
    ["await", "yield"],
    ["promise", "future", "task"],
    ["thread", "worker"],
    ["process", "proc"],
    ["goroutine", "coroutine"],
    ["fiber", "greenlet"],
    ["queue", "channel"],
    ["mq", "message queue"],
    ["pub sub", "event bus"],
    ["event", "message", "notification"],
    ["producer", "publisher"],
    ["consumer", "subscriber"],
    ["schedule", "cron"],
    ["timer", "timeout"],
    ["interval", "period"],
    ["debounce", "throttle"],
    ["batch", "bulk"],
    ["parallel", "concurrent"],
    ["serial", "sequential"],
    ["atomic", "transactional"],
    ["race", "hazard"],
    -- Frontend / UI
    ["ui", "interface"],
    ["ux", "experience"],
    ["frontend", "client"],
    ["backend", "server"],
    ["view", "screen", "page"],
    ["component", "widget"],
    ["element", "node"],
    ["button", "btn"],
    ["input", "field"],
    ["form", "survey"],
    ["modal", "dialog"],
    ["popup", "popover"],
    ["toast", "snackbar"],
    ["tooltip", "hint"],
    ["dropdown", "select"],
    ["checkbox", "toggle"],
    ["radio", "choice"],
    ["tab", "panel"],
    ["nav", "navigation"],
    ["menu", "nav"],
    ["sidebar", "drawer"],
    ["header", "toolbar"],
    ["footer", "bottom"],
    ["layout", "template"],
    ["theme", "skin"],
    ["style", "css"],
    ["class", "classname"],
    ["responsive", "adaptive"],
    ["mobile", "phone"],
    ["desktop", "web"],
    ["a11y", "accessibility"],
    ["i18n", "internationalization"],
    ["l10n", "localization"],
    ["rtl", "bidi"],
    ["route", "navigation"],
    ["redirect", "navigate"],
    ["render", "paint", "draw"],
    ["hydrate", "mount"],
    ["unmount", "dispose"],
    ["state", "store"],
    ["prop", "property"],
    ["hook", "effect"],
    ["asset", "resource"],
    -- Files / paths / IO
    ["fs", "filesystem"],
    ["file", "document"],
    ["dir", "directory", "folder"],
    ["path", "filepath"],
    ["filename", "basename"],
    ["ext", "extension"],
    ["tmp", "temp", "temporary"],
    ["stdin", "input"],
    ["stdout", "output"],
    ["stderr", "error"],
    ["io", "inputoutput"],
    ["read", "load"],
    ["write", "save"],
    ["open", "load"],
    ["close", "dispose"],
    ["flush", "sync"],
    ["stream", "pipe"],
    ["archive", "zip", "tar"],
    ["compress", "gzip"],
    ["extract", "unzip"],
    ["upload", "import"],
    ["download", "export"],
    -- Domain-ish but common in apps
    ["email", "mail"],
    ["sms", "text"],
    ["phone", "tel"],
    ["address", "addr"],
    ["profile", "account"],
    ["avatar", "picture"],
    ["image", "img", "picture"],
    ["photo", "image"],
    ["video", "media"],
    ["audio", "sound"],
    ["payment", "billing"],
    ["invoice", "bill"],
    ["price", "cost", "amount"],
    ["currency", "money"],
    ["order", "purchase"],
    ["cart", "basket"],
    ["checkout", "payment"],
    ["subscription", "plan"],
    ["notification", "alert"],
    ["message", "msg"],
    ["comment", "reply"],
    ["rating", "score"],
    ["favorite", "bookmark"],
    ["tag", "label"],
    ["category", "group"],
    ["org", "organization", "company"],
    ["team", "group"],
    ["member", "user"],
    ["admin", "administrator"],
    ["owner", "maintainer"],
    ["customer", "client"],
    ["tenant", "workspace"],
    -- Time / dates
    ["time", "timestamp"],
    ["date", "day"],
    ["datetime", "timestamp"],
    ["tz", "timezone"],
    ["utc", "gmt"],
    ["epoch", "unix"],
    ["duration", "interval"],
    ["ttl", "expiry", "expiration"],
    ["expire", "invalidate"],
    ["created", "inserted"],
    ["updated", "modified"],
    ["deleted", "removed"],
    ["archived", "hidden"],
    ["now", "current"],
    ["previous", "prev"],
    ["next", "following"],
    -- Boolean / status naming
    ["active", "enabled"],
    ["inactive", "disabled"],
    ["visible", "shown"],
    ["hidden", "invisible"],
    ["valid", "ok"],
    ["invalid", "bad"],
    ["available", "ready"],
    ["unavailable", "offline"],
    ["pending", "waiting"],
    ["complete", "done", "finished"],
    ["cancel", "abort"],
    ["cancelled", "aborted"],
    ["open", "active"],
    ["closed", "resolved"],
    ["draft", "unpublished"],
    ["published", "live"],
    ["private", "internal"],
    ["public", "external"],
    -- Math / algorithms / general CS
    ["algo", "algorithm"],
    ["calc", "calculate", "compute", "evaluate", "eval"],
    ["sum", "total"],
    ["avg", "average", "mean"],
    ["min", "minimum"],
    ["max", "maximum"],
    ["count", "size", "length"],
    ["len", "length"],
    ["pos", "position", "index"],
    ["coord", "coordinate"],
    ["x", "horizontal"],
    ["y", "vertical"],
    ["graph", "network"],
    ["node", "vertex"],
    ["edge", "link"],
    ["tree", "hierarchy"],
    ["parent", "ancestor"],
    ["child", "descendant"],
    ["root", "base"],
    ["leaf", "terminal"],
    ["hash", "checksum"],
    ["uuid", "identifier"],
    ["random", "rand"],
    ["shuffle", "permute"],
    ["sample", "example"],
    ["threshold", "limit"],
    ["range", "span"],
    ["bound", "limit"],
    -- Language / ecosystem abbreviations
    ["js", "javascript"],
    ["ts", "typescript"],
    ["py", "python"],
    ["rb", "ruby"],
    ["rs", "rust"],
    ["go", "golang"],
    ["kt", "kotlin"],
    ["cs", "csharp"],
    ["cpp", "cplusplus"],
    ["objc", "objectivec"],
    ["hs", "haskell"],
    ["ex", "elixir"],
    ["erl", "erlang"],
    ["sh", "shell", "bash"],
    ["ps", "powershell"],
    ["repl", "console"],
    ["npm", "node"],
    ["yarn", "pnpm"],
    ["pip", "python"],
    ["gem", "ruby"],
    ["cargo", "rust"],
    ["maven", "gradle"],
    ["sbt", "scala"],
    -- Architecture / design
    ["arch", "architecture"],
    ["design", "blueprint"],
    ["pattern", "idiom"],
    ["layer", "tier"],
    ["adapter", "wrapper"],
    ["facade", "gateway"],
    ["factory", "builder"],
    ["singleton", "instance"],
    ["strategy", "policy"],
    ["observer", "listener"],
    ["listener", "subscriber"],
    ["event", "signal"],
    ["command", "action"],
    ["query", "read model"],
    ["cqrs", "command query"],
    ["ddd", "domain driven"],
    ["aggregate", "root"],
    ["service", "manager"],
    ["manager", "coordinator"],
    ["orchestrator", "coordinator"],
    ["scheduler", "planner"],
    ["worker", "processor"],
    ["executor", "runner"],
    ["parser", "reader"],
    ["writer", "emitter"],
    ["adapter", "connector"],
    ["plugin", "extension"],
    ["feature", "capability"],
    ["flag", "toggle"],
    -- Documentation / project management
    ["doc", "documentation"],
    ["readme", "doc"],
    ["guide", "manual"],
    ["example", "sample"],
    ["demo", "example"],
    ["tutorial", "guide"],
    ["note", "comment"],
    ["issue", "ticket"],
    ["task", "todo"],
    ["story", "requirement"],
    ["epic", "initiative"],
    ["milestone", "checkpoint"],
    ["roadmap", "plan"],
    ["owner", "assignee"],
    ["review", "audit"],
    ["approve", "accept"],
    ["reject", "decline"],
    ["blocker", "dependency"],
    ["priority", "severity"]
  ]

builtInSynonymLexicon :: SynonymLexicon
builtInSynonymLexicon =
  case compileSynonymGroups builtInSynonymGroups of
    Right lexicon ->
      lexicon
    Left errors ->
      error (T.unpack (renderInvalidBuiltInSynonymGroups errors))

compileSynonymGroups ::
  [[Text]] ->
  Either (NonEmpty SynonymGroupError) SynonymLexicon
compileSynonymGroups groups =
  case validationErrors of
    [] ->
      Right (SynonymLexicon (buildAdjacency normalizedGroups))
    firstError : restErrors ->
      Left (firstError NE.:| restErrors)
  where
    validatedGroups =
      zipWith validateGroup [1 ..] groups

    validationErrors =
      concat
        [ errors
        | Left errors <- validatedGroups
        ]

    normalizedGroups =
      [ terms
      | Right terms <- validatedGroups
      ]

mergeSynonymLexicons :: SynonymLexicon -> SynonymLexicon -> SynonymLexicon
mergeSynonymLexicons =
  (<>)

directSynonyms :: SynonymLexicon -> SynonymTerm -> Set.Set SynonymTerm
directSynonyms lexicon token =
  case lexicon of
    SynonymLexicon adjacency ->
      Map.findWithDefault Set.empty token adjacency

validateGroup :: Int -> [Text] -> Either [SynonymGroupError] [SynonymTerm]
validateGroup groupIndex groupTerms =
  case tooFewTermsError ++ termErrors ++ tooFewDistinctError of
    [] ->
      Right distinctTerms
    errors ->
      Left errors
  where
    tooFewTermsError =
      [ SynonymGroupHasTooFewTerms groupIndex
      | length groupTerms < 2
      ]

    validatedTerms =
      zipWith (validateTerm groupIndex) [1 ..] groupTerms

    termErrors =
      [ err
      | Left err <- validatedTerms
      ]

    normalizedTerms =
      [ term
      | Right token <- validatedTerms,
        let term = token
      ]

    distinctTerms =
      Set.toList (Set.fromList normalizedTerms)

    tooFewDistinctError =
      [ SynonymGroupHasTooFewDistinctTerms
          groupIndex
          distinctTerms
      | null termErrors,
        length groupTerms >= 2,
        length distinctTerms < 2
      ]

validateTerm :: Int -> Int -> Text -> Either SynonymGroupError SynonymTerm
validateTerm groupIndex termIndex term =
  case compileSynonymTerm term of
    Left (SynonymTermProducesNoTokens _) ->
      Left
        ( SynonymTermProducesNoToken
            groupIndex
            termIndex
            term
        )
    Right synonymTerm ->
      Right synonymTerm

compileSynonymTerm :: Text -> Either SynonymTermError SynonymTerm
compileSynonymTerm term =
  case NE.nonEmpty (map canonicalizeSearchToken (tokenizeSearchText term)) of
    Nothing -> Left (SynonymTermProducesNoTokens term)
    Just tokens -> Right (SynonymTerm tokens)

buildAdjacency :: [[SynonymTerm]] -> Map.Map SynonymTerm (Set.Set SynonymTerm)
buildAdjacency =
  foldl' insertGroup Map.empty
  where
    insertGroup acc groupTerms =
      case Set.toList (Set.fromList groupTerms) of
        [] ->
          acc
        uniqueTerms ->
          foldl'
            ( \mapAcc term ->
                Map.insertWith Set.union term (Set.delete term (Set.fromList uniqueTerms)) mapAcc
            )
            acc
            uniqueTerms

renderInvalidBuiltInSynonymGroups :: NonEmpty SynonymGroupError -> Text
renderInvalidBuiltInSynonymGroups errors =
  "Invalid built-in symbol-search synonym groups:\n"
    <> T.intercalate "\n" (map renderSynonymGroupError (NE.toList errors))

renderSynonymGroupError :: SynonymGroupError -> Text
renderSynonymGroupError = \case
  SynonymGroupHasTooFewTerms synonymGroupIndex ->
    "Invalid lore.yaml symbol-search synonym group "
      <> T.pack (show synonymGroupIndex)
      <> ": each synonym group must contain at least two entries."
  SynonymTermProducesNoToken synonymGroupIndex synonymTermIndex invalidSynonymTerm ->
    "Invalid lore.yaml symbol-search synonym group "
      <> T.pack (show synonymGroupIndex)
      <> ", entry "
      <> T.pack (show synonymTermIndex)
      <> ": "
      <> quote invalidSynonymTerm
      <> " produces no search token. Each synonym entry must represent at least one token."
  SynonymGroupHasTooFewDistinctTerms synonymGroupIndex normalizedTerms ->
    "Invalid lore.yaml symbol-search synonym group "
      <> T.pack (show synonymGroupIndex)
      <> ": entries collapse to fewer than two distinct search terms after normalization: "
      <> T.intercalate ", " (map renderSynonymTerm normalizedTerms)
      <> "."

renderSynonymTerm :: SynonymTerm -> Text
renderSynonymTerm term =
  case term of
    SynonymTerm tokens ->
      "[" <> T.intercalate ", " (map unSearchToken (NE.toList tokens)) <> "]"

quote :: Text -> Text
quote value =
  "\"" <> value <> "\""

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = List.foldl'
