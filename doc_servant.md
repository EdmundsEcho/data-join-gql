
# Servant

## Handler
```haskell
λ> :info Handler
type role Handler nominal
newtype Handler a
  = Handler {runHandler' :: Control.Monad.Trans.Except.ExceptT
                              ServerError ghc-prim-0.5.3:GHC.Types.IO a}
        -- Defined in ‘Servant.Server.Internal.Handler’

-- aka
newtype Handler a = Handler { runHandler' :: ExceptT ServerError IO a }

```
Where ExceptT...
```
-- from the 'mtl' package at
newtype ExceptT e m a = ExceptT (m (Either e a))
```
### Performing IO

`MonadIO Handler` from the `Handler` instance for `MonadIO IO`
where `MonadIO`
```
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```
_SO_ use `liftIO` to perform io in my handlers.
```
-- has ServerT
type IOAPI = "myfile.txt" :> Get '[JSON] FileContent

-- outside world -> Haskell model
newtype FileContent = FileContent
  { content :: String }
  deriving Generic

-- required to move content into Servant handlers
instance ToJSON FileContent

-- has Handler
-- recall, where Servant ServerT -> Handler
server :: Server IOAPI
server = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)
```

## Understanding API -> Handler
1. API instance of `HasServer` -> instance `Handler`
2. Match pattern on types are used to select which instance
3. `type Server api = ServerT api Handler`
    * `ServerT` is thus a type family that computes the required types for the handlers; this is part of the `HasServer` type class.

### How to use another `monad`
* The default monad is `Handler`
* `ServerT` takes `Handler` aka a monad, to describe the type of the final computed result

```
   Handler :: ExceptT ServerError IO a -> Handler a

   ServerT (Get '[JSON] Person) SomeMonad
   => SomeMonad Person
```
Key question: How get the `SomeMonad` -> `Something Servant can understand`?
Answer:
```
   hoistServer :: Api
               -> (Nat m -> n)
               -> ServerT someapi m
               -> ServerT someapi n

   where n :: Handler
   ```

### Inspect the type of my api (`GraphiQL`) constructed using `ServerT`

```haskell
λ> :kind! ServerT GraphiQL Handler
ServerT GraphiQL Handler :: Type
= (GQLRequest -> Handler GQLResponse)
  :<|> Tagged Handler Application
```

ServeT is a type family.  It essentially pattern matches to construct types.

```haskell
λ> :info ServerT
class HasServer (api :: k)
                (context :: [Type]) where
  type family ServerT (api :: k) (m :: Type -> Type) :: Type
  ...
        -- Defined in ‘Servant.Server.Internal’
```

Task: What is `HasServer` instance for my api?
Given
```haskell
λ> :info GraphiQL
type GraphiQL =
  ("graphiql" :> ("v1" :> ServeGql))
  :<|> ("graphiql" :> ("v1" :> ServeGraphi))
        -- Defined at src/Api/GraphiQL.hs:17:1
```
My type is a synonym.  What is the underlying type? To figure this out, we will trace our way down the type class function calls.

```haskell
instance
  ( KnownSymbol path
  , HasServer api context
  ) =>
    HasServer (path :> api) context
```

In the documents

```haskell
type instance
  ServerT (path :> api) m =
    ServerT api m
```

Note the following:
1. `ServerT` is called recursively
2. The repeated call does not include `path`

So where...

```haskell
path :: "v1"
api :: ServeGql
m :: Handler
```
The next iteration calls on another instance of `HasServer`.

```haskell
type instance
  ServerT (a :<|> b) m =
    ServerT a m :<|> ServerT b m
```

```haskell
λ> :info ServeGql
type ServeGql =
  ReqBody '[JSON] GQLRequest :> Post '[JSON] GQLResponse
        -- Defined at src/Api/GraphiQL.hs:22:1

λ> :info ReqBody
type ReqBody =
  ReqBody' '[Required, Strict] :: [Type] -> Type -> Type
        -- Defined in ‘servant-0.16.2:Servant.API.ReqBody’

λ> :info Post
type Post = Verb 'POST 200 :: [Type] -> Type -> Type
        -- Defined in ‘servant-0.16.2:Servant.API.Verbs’
```

… going down the right side of the expression we have `Verb`.  This a general, and widely used type within the package.

```
λ> :info Verb
… => HasServer (Verb method status ctypes (Headers h a)) context
… => HasServer (Verb method status ctypes a) context

```

With the `HasServer` instance, we can find the corresponding `ServerT`:

```
type instance
  ServerT (Verb method status ctypes a) m = m a
```

...where the right side of `ServeGql`:
```
Post '[JSON] GQLResponse
 =
Verb 'POST 200 '[JSON] GQLResponse
```

```
λ> :info Capture
type Capture =
  Capture' '[] :: ghc-prim-0.5.3:GHC.Types.Symbol -> Type -> Type
        -- Defined in ‘Servant.API.Capture’
```
   => Capture is a type synonym for `Capture' '[]`
   … defined in 'Servant.API.Capture'

```
λ> import Servant.API.Capture
(0.00 secs, 0 bytes)

λ> :info Capture'
type role Capture' phantom phantom phantom
data Capture' (mods :: [Type])
              (sym :: ghc-prim-0.5.3:GHC.Types.Symbol)
              a
        -- Defined in ‘Servant.API.Capture’
instance (ToHttpApiData v, HasLink sub) =>
         HasLink (Capture' mods sym v :> sub)
  -- Defined in ‘Servant.Links’

instance (GHC.TypeLits.KnownSymbol capture, FromHttpApiData a,
          HasServer api context) =>
         HasServer (Capture' mods capture a :> api) context
  -- Defined in ‘Servant.Server.Internal’

type instance MkLink (Capture' mods sym v :> sub) a
  = v -> MkLink sub a
        -- Defined in ‘Servant.Links’

type instance ServerT (Capture' mods capture a :> api) m
  = a -> ServerT api m
        -- Defined in ‘Servant.Server.Internal’

```

What does `HasServer` type tell us?
```
HasServer (Capture' mods capture a :> api) context

```
Where
```
mods :: '[]
capture :: "username"
a :: String
api :: Get '[JSON] User
m :: Handler

-- means we have a function!
String -> ServerT (Get '[JSON] User) Handler
```

How `Servant` goes from the `Api` into the type for the server. The result is __a recipe__ for how to implement the server for the `Api`!

```
ServerT UsersAPI Handler
ServerT ("users" :> (UsersIndex :<|> UsersShow)) Handler
ServerT (UsersIndex :<|> UsersShow) Handler
ServerT UsersIndex Handler :<|> ServerT UsersShow Handler
ServerT (Get '[JSON] [User]) Handler :<|> ServerT UsersShow Handler
ServerT (Verb GET 200 '[JSON] [User]) Handler :<|> ServerT UsersShow Handler
Handler [User] :<|> ServerT UsersShow Handler
Handler [User] :<|> ServerT (Capture "username" String :> Get '[JSON] User) Handler
Handler [User] :<|> (String -> ServerT (Get '[JSON] User)) Handler
Handler [User] :<|> (String -> ServerT (Verb GET 200 '[JSON] User)) Handler
Handler [User] :<|> (String -> Handler User)
```
Why the ToJSON constraints?

How pattern matching for instances are used to split and terminate with iteration. Here are the two instances of `AllCTRender` (something that our `a` must implement)
```
instance
  ( Accept ct
  , AllMime cts
  , AllMimeRender (ct : cts) a
  ) =>
    AllCTRender (ct : cts) a

instance
  (TypeError ...) =>
    AllCTRender '[] ()
```

Another example `AllMimeRender`

```
instance
  ( MimeRender ctyp a
  ) =>
    AllMimeRender '[ctyp] a

instance
  ( MimeRender ctyp a
  , AllMimeRender (ctyp' : ctyps) a
  ) =>
    AllMimeRender (ctyp : ctyp' : ctyps) a
```
The API only speaks JSON represented by `'[JSON]`… a match for the first instance of the type class `AllMimeRender`. So, JSON needs to implement `MimeRender`.  Key: note how we matched with the expression to the right of `=>`, and hunt using what it needs to implement on the left.

```
λ> :info MimeRender
...
instance [overlappable] aeson-1.4.6.0:Data.Aeson.Types.ToJSON.ToJSON
                          a =>
                        MimeRender JSON a
  -- Defined in ‘Servant.API.ContentTypes’
...
```
Recall: GHC selects which instance *before* considering the constraints.
Trick... this will changes which instance GHC will select... based a generic a.
```
instance a ~ () => IsString (Writer String a)where
  fromString = tell
```


Here's how it all goes.

* In order for there to be an instance of `HasServer` for `Verb GET 200 '[JSON] User` there has to be an instance of `AllCTRender` for `'[JSON]` and `User`
* In order for there to be an instance of `AllCTRender` for '[JSON] and `User` there has to be an instance of `AllMimeRender` for '[JSON] and `User`
* In order for there to be an instance of `AllMimeRender` for `'[JSON]` and `User` there has to be an instance of `MimeRender` for `JSON` and `User`
* In order for there to be an instance of `MimeRender` for `JSON` and `User` there has to be an instance of Aeson's `ToJSON` for `User`

So that's why when we tried to apply `UsersAPI` to serve we needed to define a `ToJSON` instance for User.

Using `enter`
```haskell
type PersonAPI =
    "users" :> Capture "name" String :> Get '[JSON] Person
   -- NEW: removed Raw from here

-- NEW
type WholeAPI = PersonAPI :<|> Raw

type AppM = ReaderT Config (EitherT ServantErr IO)

userAPI :: Proxy PersonAPI
userAPI = Proxy

-- NEW
wholeAPI :: Proxy WholeAPI
wholeAPI = Proxy

-- NEW: changed 'userAPI' to 'wholeAPI'
app :: Config -> Application
app cfg = serve wholeAPI (readerServer cfg)

readerServer :: Config -> Server WholeAPI
readerServer cfg = enter (readerToEither cfg) server
              :<|> S.serveDirectory "/static" -- NEW

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT PersonAPI AppM
server = singlePerson

singlePerson :: String -> AppM Person
singlePerson str = do
    let person = Person { name = "Joe", email = "joe@example.com" }
    return person

```
