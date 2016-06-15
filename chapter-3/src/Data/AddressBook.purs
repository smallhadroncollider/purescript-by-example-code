module Data.AddressBook where

import Prelude

import Data.List
import Data.Maybe

import Control.Plus (empty)

type Entry =
    {
        firstName :: String,
        lastName :: String,
        address :: Address
    }

type Address =
    {
        street :: String,
        city :: String,
        state :: String
    }

type AddressBook = List Entry


showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <> address.city <> ", " <> address.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

filterEntry :: String -> String -> Entry -> Boolean
filterEntry firstName lastName entry = entry.firstName == firstName && entry.lastName == lastName

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter (filterEntry firstName lastName)

entryExists :: String -> String -> AddressBook -> Boolean
entryExists firstName lastName = not <<< null <<< filter (filterEntry firstName lastName)

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy duplicates
    where
        duplicates :: Entry -> Entry -> Boolean
        duplicates a b = a.firstName == b.firstName && a.lastName == b.lastName
