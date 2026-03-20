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
-- | Uses the Contents API with raw media type to
-- | get the file as plain text (CORS-friendly).
fetchFile
  :: Config
  -> String
  -> Aff (Either String String)
fetchFile cfg path = do
  let
    url =
      "https://api.github.com/repos/"
        <> cfg.owner
        <> "/"
        <> cfg.repo
        <> "/contents/"
        <> path
        <> "?ref="
        <> cfg.ref
  result <- try do
    resp <- fetch url
      { headers:
          { "Authorization":
              "Bearer " <> cfg.token
          , "Accept":
              "application/vnd.github.raw+json"
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
