module Data.AddressBook.Validation where

import Prelude
import Data.String.Regex as R
import Data.Either (Either(..))
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe
import Data.Validation.Semigroup (V, unV, invalid)
import Control.Apply
import Control.Applicative
import Data.Traversable
import Data.AddressBook

type Errors = Array String

phoneNumberRegex :: R.Regex
phoneNumberRegex =
  unsafePartial
    case R.regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
      Right r -> r

stateRegex :: R.Regex
stateRegex = 
  unsafePartial
    case R.regex "^[A-Z]{2}$" noFlags of
      Right r -> r

nonEmptyRegex :: R.Regex
nonEmptyRegex = 
  unsafePartial
    case R.regex "[^\\s]" noFlags of
      Right r -> r

matches :: String -> R.Regex -> String -> V Errors Unit
matches _ regex value | R.test regex value =
  pure unit
matches field _ _ =
  invalid 
    ["Field '" <> field <> "' did not match the required format"]

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field value = matches field nonEmptyRegex value

validateAddress :: Address -> V Errors Address
validateAddress (Address x) = 
  address <$> 
    pure x.street <*> 
    pure x.city <*> 
    (matches "state" stateRegex x.state *> pure x.state)

--validatePerson :: Person -> V Errors Person
--validatePerson (Person x) =
--  person <$>
--    pure x.firstName <*>
--    pure x.lastName <*>
--    validateAddress x.homeAddress <*>
--    pure x.phones

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid ["Field '" <> field <> "' must contain at least one value"]
arrayNonEmpty _     _  = pure unit

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber o) =
  phoneNumber <$> pure o.phoneType
              <*> (matches "Number" phoneNumberRegex o.number *> pure o.number)

validatePhoneNumber' :: String -> Either Errors String
validatePhoneNumber' number =
  unV Left Right $
    (matches "Number" phoneNumberRegex number *> pure number)

validatePerson :: Person -> V Errors Person
validatePerson (Person o) =
  person <$> (nonEmpty "First Name" o.firstName *> pure o.firstName)
         <*> (nonEmpty "Last Name"  o.lastName  *> pure o.lastName)
         <*> validateAddress o.homeAddress
         <*> (arrayNonEmpty "Phone Numbers" o.phones *> traverse validatePhoneNumber o.phones)

validateFirstName' :: String -> Either Errors String
validateFirstName' firstName =
  unV Left Right $ 
    (nonEmpty "First Name" firstName *> pure firstName)

validateLastName' :: String -> Either Errors String
validateLastName' lastName =
  unV Left Right $ 
    (nonEmpty "Last Name" lastName *> pure lastName)

validateAddress' :: Person -> Either Errors Address
validateAddress' (Person o) =
  unV Left Right $ validateAddress o.homeAddress

validateState' :: String -> Either Errors String
validateState' state = 
  unV Left Right $ (matches "state" stateRegex state *> pure state)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p
