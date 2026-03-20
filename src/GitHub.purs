-- | Fetch files from a GitHub repository via the
-- | Contents API.
module GitHub
  ( fetchFile
  , Config
  ) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, try)
import Effect.Exception (message)
import Fetch (fetch)

-- | GitHub connection config.
type Config =
  { owner :: String
  , repo :: String
  , ref :: String
  , token :: String
  }

-- | Fetch a file's text content from a GitHub repo.
-- | Uses the raw content endpoint for simplicity.
fetchFile
  :: Config
  -> String
  -> Aff (Either String String)
fetchFile cfg path = do
  let
    url =
      "https://raw.githubusercontent.com/"
        <> cfg.owner
        <> "/"
        <> cfg.repo
        <> "/"
        <> cfg.ref
        <> "/"
        <> path
  result <- try do
    resp <- fetch url
      { headers:
          { "Authorization":
              "Bearer " <> cfg.token
          , "Accept": "application/vnd.github.raw"
          }
      }
    body <- resp.text
    pure { status: resp.status, body }
  case result of
    Left err -> pure $ Left (message err)
    Right r
      | r.status == 200 ->
          pure $ Right r.body
      | r.status == 404 ->
          pure $ Left
            ("Not found: " <> path)
      | otherwise ->
          pure $ Left
            ( "HTTP " <> show r.status
                <> " fetching "
                <> path
            )
