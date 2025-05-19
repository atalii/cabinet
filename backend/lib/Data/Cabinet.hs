{-# LANGUAGE NamedFieldPuns #-}

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
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as V4

-- A FileBuf contains a single file and associated metadata.
data FileBuf = FileBuf
  { f_name :: T.Text,
    -- Scotty gives out uploaded file types as bytestrings rather than as texts,
    -- presumably in case the uploaded data doesn't parse well as any usual encoding.
    f_content_type :: B.ByteString,
    f_sticky :: Bool,
    f_public :: Bool,
    f_created_at :: UTCTime,
    f_data :: B.ByteString
  }

-- A FilePool contains and indexes various FileStores.
type FilePool = TVar FilePool'

-- Keep track of a number of FileStores, recording how much data is in use and
-- when to run GC.
--
-- The GC algorithm is subject to change, but as is, removes the oldest p_gc_prop percent of
-- non-sticky files after p_in_use has gone up p_gc_interval bytes since last
-- recorded.
data FilePool' = FilePool'
  { p_by_uuid :: M.Map UUID.UUID FileBuf,
    p_by_date :: M.Map UTCTime UUID.UUID,
    p_md :: Metadata
  }

data Metadata = Metadata
  { m_in_use :: Int,
    m_in_use_at_last_gc :: Int,
    m_gc_interval :: Int,
    m_gc_prop :: Int
  }

-- An index type is a snapshot of a pool at a given time. There's no guarantee
-- that this is up to date.
type Index = [IndexEntry]

data IndexEntry = IndexEntry
  { i_name :: T.Text,
    i_mime_type :: B.ByteString,
    i_id :: UUID.UUID,
    i_creation :: UTCTime,
    i_public :: Bool,
    i_sticky :: Bool
  }

newPool :: IO FilePool
newPool =
  newTVarIO $
    FilePool'
      { p_by_uuid = M.empty,
        p_by_date = M.empty,
        p_md =
          Metadata
            { m_in_use = 0,
              m_in_use_at_last_gc = 0,
              -- By default, remove the oldest 10% at each megabyte. Perhaps this is stupid.
              --
              -- TODO: Make this configurable.
              m_gc_interval = 1024 * 1024,
              m_gc_prop = 10
            }
      }

-- Create a FileStore object given a name, whether to keep it
-- around past GC,G and its contetns.
buildFile :: T.Text -> B.ByteString -> Bool -> B.ByteString -> IO FileBuf
buildFile f_name f_content_type f_sticky f_data =
  getCurrentTime >>= \f_created_at ->
    return $
      FileBuf
        { f_name,
          f_content_type,
          f_sticky,
          f_public = False,
          f_created_at,
          f_data
        }

-- Set the sticky flag on a given file.
setSticky :: FilePool -> UUID.UUID -> Bool -> IO ()
setSticky fp target val = atomically $ modifyTVar fp runSetPublic
  where
    runSetPublic :: FilePool' -> FilePool'
    runSetPublic fp' =
      FilePool'
        { p_by_uuid = M.adjust setPublic' target (p_by_uuid fp'),
          p_by_date = p_by_date fp',
          p_md = p_md fp'
        }

    setPublic' :: FileBuf -> FileBuf
    setPublic' fb =
      FileBuf
        { f_name = f_name fb,
          f_content_type = f_content_type fb,
          f_sticky = val,
          f_public = f_public fb,
          f_created_at = f_created_at fb,
          f_data = f_data fb
        }

-- Set the public flag on a given file.
setPublic :: FilePool -> UUID.UUID -> Bool -> IO ()
setPublic fp target val = atomically $ modifyTVar fp runSetPublic
  where
    runSetPublic :: FilePool' -> FilePool'
    runSetPublic fp' =
      FilePool'
        { p_by_uuid = M.adjust setPublic' target (p_by_uuid fp'),
          p_by_date = p_by_date fp',
          p_md = p_md fp'
        }

    setPublic' :: FileBuf -> FileBuf
    setPublic' fb =
      FileBuf
        { f_name = f_name fb,
          f_content_type = f_content_type fb,
          f_sticky = f_sticky fb,
          f_public = val,
          f_created_at = f_created_at fb,
          f_data = f_data fb
        }

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
       in if m_in_use (p_md new) - m_in_use (p_md old) >= m_gc_interval (p_md new)
            then runGc' new
            else new

    addToPool' :: UUID.UUID -> FilePool' -> FilePool'
    addToPool' uuid fp' =
      FilePool'
        { p_by_uuid = Map.insert uuid fb (p_by_uuid fp'),
          p_by_date = Map.insert (f_created_at fb) uuid (p_by_date fp'),
          p_md =
            Metadata
              { m_in_use = m_in_use (p_md fp') + B.length (f_data fb),
                m_in_use_at_last_gc = m_in_use_at_last_gc (p_md fp'),
                m_gc_interval = m_gc_interval (p_md fp'),
                m_gc_prop = m_gc_prop (p_md fp')
              }
        }

poolCount :: FilePool -> IO Int
poolCount fp = atomically $ fmap (M.size . p_by_uuid) (readTVar fp)

poolLookup :: FilePool -> UUID.UUID -> IO (Maybe (IndexEntry, B.ByteString))
poolLookup fp uuid = atomically $ fmap poolLookup' (readTVar fp)
  where
    poolLookup' :: FilePool' -> Maybe (IndexEntry, B.ByteString)
    poolLookup' fp' = do
      file <- M.lookup uuid $ p_by_uuid fp'
      return (toEntry (uuid, file), f_data file)

poolMetadata :: FilePool -> IO Metadata
poolMetadata fp = atomically $ fmap p_md (readTVar fp)

runGc :: FilePool -> IO ()
runGc fp = atomically $ modifyTVar fp runGc'

runGc' :: FilePool' -> FilePool'
runGc' fp' =
  let by_date = p_by_date fp'
      dates = Map.keys by_date
      numToClean = length dates * m_gc_prop (p_md fp') `div` 100
      (targets, p_by_date') = deleteOldest numToClean by_date
      (p_by_uuid', saved) = deleteAll targets (p_by_uuid fp')
   in FilePool'
        { p_by_uuid = p_by_uuid',
          p_by_date = p_by_date',
          p_md =
            Metadata
              { m_in_use = m_in_use (p_md fp') - saved,
                m_in_use_at_last_gc = m_in_use (p_md fp'),
                m_gc_interval = m_gc_interval (p_md fp'),
                m_gc_prop = m_gc_prop (p_md fp')
              }
        }
  where
    deleteAll :: (Ord k) => [k] -> Map.Map k FileBuf -> (Map.Map k FileBuf, Int)
    deleteAll [] m = (m, 0)
    deleteAll (x : xs) m =
      let size = B.length $ f_data $ m Map.! x
          m' = Map.delete x m
          (m'', acc) = deleteAll xs m'
       in (m'', acc + size)

    deleteOldest :: Int -> Map.Map k v -> ([v], Map.Map k v)
    deleteOldest 0 m = ([], m)
    deleteOldest n m =
      let ((_, v), m') = Map.deleteFindMin m
          (targets, m'') = deleteOldest (n - 1) m'
       in (v : targets, m'')

poolIndex :: FilePool -> IO Index
poolIndex fp = atomically $ fmap poolIndex' (readTVar fp)
  where
    poolIndex' fp' = fmap toEntry ((M.assocs . p_by_uuid) fp')

toEntry :: (UUID.UUID, FileBuf) -> IndexEntry
toEntry (uuid, f) =
  IndexEntry
    { i_name = f_name f,
      i_mime_type = f_content_type f,
      i_id = uuid,
      i_creation = f_created_at f,
      i_public = f_public f,
      i_sticky = f_sticky f
    }
