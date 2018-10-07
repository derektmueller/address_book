module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe(..))
import Control.Apply

type Entry = 
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

newtype Person = Person
  { firstName   :: String
  , lastName    :: String
  , homeAddress :: Address
  , phones      :: Array PhoneNumber
  }

examplePerson :: Person
examplePerson = 
  person "some-first-name" "some-last-name" 
    ((Address { street: "123 ABC", city: "Santa Cruz", state: "CA" })) 
      [
        PhoneNumber {phoneType: HomePhone, number: "1-123-1234-1234"},
        PhoneNumber {phoneType: WorkPhone, number: "1-123-1234-1234"}
      ]

person :: 
  String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones =
  Person { firstName, lastName, homeAddress, phones }

newtype Address = Address
  { street :: String
  , city :: String
  , state :: String }

instance showAddress :: Show Address where
  show (Address o) = "Address " <>
    "{ street: " <> show o.street <>
    ", city: "   <> show o.city <>
    ", state: "  <> show o.state <>
    " }"

type AddressBook = List Entry

newtype PhoneNumber = PhoneNumber
  { phoneType :: PhoneType,
    number :: String
  }

data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone

instance eqPhoneType :: Eq PhoneType where
  eq HomePhone HomePhone = true
  eq WorkPhone WorkPhone = true
  eq CellPhone CellPhone = true
  eq OtherPhone OtherPhone = true
  eq _ _ = false

instance showPhoneType :: Show PhoneType where
  show HomePhone = "Home Phone"
  show WorkPhone = "Work Phone"
  show CellPhone = "Cell Phone"
  show OtherPhone = "Other Phone"

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber phoneType number = PhoneNumber { phoneType, number }

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

--showEntry :: Entry -> String
--showEntry entry = 
--  entry.lastName <> ", " <>
--  entry.firstName <> ": " <>
--  show entry.address

--findEntry :: String -> String -> AddressBook -> Maybe Entry
--findEntry firstName lastName book = 
--  head $ filter (\entry -> entry.firstName == firstName && entry.lastName == lastName) book
--
--person1 = { 
--  firstName: "test", lastName: "test", address: { street: "123", city: "abc", state: "CA" }}
--person2 = { 
--  firstName: "test2", lastName: "test2", address: { street: "1234", city: "abc", state: "CA" }}
--person3 = { 
--  firstName: "test2", lastName: "test2", address: { street: "12345", city: "abc", state: "CA" }}
--
--book1 = insertEntry person3 $ insertEntry person2 $ insertEntry person1 emptyBook

--findEntryByStreetAddress :: String -> AddressBook -> Maybe Entry
--findEntryByStreetAddress street = 
--  head <<< filter (\entry -> entry.address.street == street)
--
--hasName :: String -> String -> AddressBook -> Boolean
--hasName firstName lastName book = 
--  not null $ filter (\entry -> entry.firstName == firstName && entry.lastName == lastName) book
--
--removeDuplicates :: AddressBook -> AddressBook
--removeDuplicates book = nubBy 
--  (\entry1 entry2 -> entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName)
--  book
