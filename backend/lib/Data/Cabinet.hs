{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Cabinet
  ( newPool,
    buildFile,
    addToPool,
    poolCount,
    poolIndex,
    poolLookup,
    poolMetadata,
    setPublic,
    setSticky,
    IndexEntry (..),
    Metadata (..),
    FilePool,
    FileBuf,
    runGc,
  )
where

import Control.Concurrent.STM
import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4

-- A FileBuf contains a single file and associated metadata.
data FileBuf = FileBuf
  { _f_name :: T.Text,
    -- Scotty gives out uploaded file types as bytestrings rather than as texts,
    -- presumably in case the uploaded data doesn't parse well as any usual encoding.
    _f_content_type :: B.ByteString,
    _f_sticky :: Bool,
    _f_public :: Bool,
    _f_created_at :: UTCTime,
    _f_data :: B.ByteString
  }

makeLenses ''FileBuf

-- A FilePool contains and indexes various FileStores.
type FilePool = TVar FilePool'

-- Keep track of a number of FileStores, recording how much data is in use and
-- when to run GC.
--
-- The GC algorithm is subject to change, but as is, removes the oldest p_gc_prop percent of
-- non-sticky files after p_in_use has gone up p_gc_interval bytes since last
-- recorded.
data FilePool' = FilePool'
  { _p_by_uuid :: M.Map UUID.UUID FileBuf,
    _p_by_date :: M.Map UTCTime UUID.UUID,
    _p_md :: Metadata
  }

data Metadata = Metadata
  { _m_in_use :: Int,
    _m_in_use_at_last_gc :: Int,
    _m_gc_interval :: Int,
    _m_gc_prop :: Int
  }

makeLenses ''Metadata
makeLenses ''FilePool'

-- An index type is a snapshot of a pool at a given time. There's no guarantee
-- that this is up to date.
type PoolIndex = [IndexEntry]

data IndexEntry = IndexEntry
  { i_name :: T.Text,
    i_mime_type :: B.ByteString,
    i_id :: UUID.UUID,
    i_creation :: UTCTime,
    i_public :: Bool,
    i_sticky :: Bool
  }

newPool :: IO FilePool
newPool = newTVarIO newPool'

newPool' :: FilePool'
newPool' =
  FilePool'
    { _p_by_uuid = M.empty,
      _p_by_date = M.empty,
      _p_md =
        Metadata
          { _m_in_use = 0,
            _m_in_use_at_last_gc = 0,
            -- By default, remove the oldest 10% at each megabyte. Perhaps this is stupid.
            --
            -- TODO: Make this configurable.
            _m_gc_interval = 1024 * 1024,
            _m_gc_prop = 10
          }
    }

-- Create a FileStore object given a name, whether to keep it
-- around past GC, and its contents.
buildFile :: T.Text -> B.ByteString -> Bool -> B.ByteString -> IO FileBuf
buildFile f_name' f_content_type' f_sticky' f_data' =
  getCurrentTime >>= \now ->
    return $
      FileBuf
        { _f_name = f_name',
          _f_content_type = f_content_type',
          _f_sticky = f_sticky',
          _f_public = False,
          _f_created_at = now,
          _f_data = f_data'
        }

-- Set the sticky flag on a given file.
setSticky :: FilePool -> UUID.UUID -> Bool -> IO ()
setSticky fp target val =
  atomically $
    modifyTVar fp $
      runOverUUID target $
        f_sticky .~ val

-- Set the public flag on a given file.
setPublic :: FilePool -> UUID.UUID -> Bool -> IO ()
setPublic fp target val =
  atomically $
    modifyTVar fp $
      runOverUUID target $
        f_public .~ val

runOverUUID :: UUID.UUID -> (FileBuf -> FileBuf) -> FilePool' -> FilePool'
runOverUUID target f = over p_by_uuid $ M.adjust f target

-- Add a FileStore object to a pool and return a unique identifier.
addToPool :: FilePool -> FileBuf -> IO UUID.UUID
addToPool fp fb =
  do
    -- Get a UUID first so as to avoid IO actions within the STM monad.
    uuid <- V4.nextRandom

    -- Then, update it with the ID.
    atomically $ modifyTVar fp (runAddition uuid)
    return uuid
  where
    runAddition :: UUID.UUID -> FilePool' -> FilePool'
    runAddition uuid old =
      let new = addToPool' uuid old
       in if new ^. (p_md . m_in_use) - old ^. (p_md . m_in_use) >= new ^. (p_md . m_gc_interval)
            then runGc' new
            else new

    addToPool' :: UUID.UUID -> FilePool' -> FilePool'
    addToPool' uuid fp' =
      fp'
        & p_by_uuid %~ Map.insert uuid fb
        & p_by_date %~ Map.insert (fb ^. f_created_at) uuid
        & p_md %~ \md ->
          md
            & m_in_use +~ B.length (fb ^. f_data)

poolCount :: FilePool -> IO Int
poolCount fp = atomically $ fmap (M.size . view p_by_uuid) (readTVar fp)

poolLookup :: FilePool -> UUID.UUID -> IO (Maybe (IndexEntry, B.ByteString))
poolLookup fp uuid = atomically $ fmap poolLookup' (readTVar fp)
  where
    poolLookup' :: FilePool' -> Maybe (IndexEntry, B.ByteString)
    poolLookup' fp' = do
      file <- M.lookup uuid $ _p_by_uuid fp'
      return (toEntry (uuid, file), _f_data file)

poolMetadata :: FilePool -> IO Metadata
poolMetadata fp = atomically $ fmap _p_md (readTVar fp)

runGc :: FilePool -> IO ()
runGc fp = atomically $ modifyTVar fp runGc'

runGc' :: FilePool' -> FilePool'
runGc' fp' =
  let by_date = fp' ^. p_by_date
      dates = Map.keys by_date
      numToClean = length dates * (fp' ^. p_md . m_gc_prop) `div` 100
      (targets, p_by_date') = deleteOldest (fp' ^. p_by_uuid) numToClean by_date
      (p_by_uuid', saved) = deleteAll targets (_p_by_uuid fp')
   in set p_by_uuid p_by_uuid' fp'
        & (over p_md (over m_in_use (subtract saved)) . over p_md (\md -> set m_in_use_at_last_gc (md ^. m_in_use) md))
  where
    deleteAll :: (Ord k) => [k] -> Map.Map k FileBuf -> (Map.Map k FileBuf, Int)
    deleteAll [] m = (m, 0)
    deleteAll (x : xs) m =
      let size = B.length $ _f_data $ m Map.! x
          m' = Map.delete x m
          (m'', acc) = deleteAll xs m'
       in (m'', acc + size)

    deleteOldest :: (Ord v) => Map.Map v FileBuf -> Int -> Map.Map k v -> ([v], Map.Map k v)
    deleteOldest idx 0 m = ([], m)
    deleteOldest idx n m =
      let ((_, v), m') = Map.deleteFindMin m
          is_sticky = (idx Map.! v) ^. f_sticky
          (targets, m'') = deleteOldest idx (n - 1) m'
       in if is_sticky then deleteOldest idx n m' else (v : targets, m'')

poolIndex :: FilePool -> IO PoolIndex
poolIndex fp = atomically $ fmap poolIndex' (readTVar fp)
  where
    poolIndex' fp' = fmap toEntry ((M.assocs . view p_by_uuid) fp')

toEntry :: (UUID.UUID, FileBuf) -> IndexEntry
toEntry (uuid, f) =
  IndexEntry
    { i_name = _f_name f,
      i_mime_type = _f_content_type f,
      i_id = uuid,
      i_creation = _f_created_at f,
      i_public = _f_public f,
      i_sticky = _f_sticky f
    }
