module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Data.AddressBook (Person(..), Address(..), PhoneNumber(..), PhoneType, examplePerson)
import Data.AddressBook.Validation (validateFirstName', validateLastName', validateState', validatePhoneNumber')
import Data.Array ((..), length, zipWith, modifyAt, filter, concat)
import React as React
import ReactDOM as ReactDOM
import React.DOM (div, form, h3', input, label, text) as DOM
import React.DOM.Props as Props
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)

type AppState = { person :: Person, errors :: Errors }

data Field =
  FirstNameField | LastNameField | StreetField | CityField |
  StateField | PhoneField PhoneType

instance eqField :: Eq Field where
  eq FirstNameField FirstNameField = true
  eq LastNameField LastNameField = true
  eq StateField StateField = true
  eq CityField CityField = true
  eq StreetField StreetField = true
  eq (PhoneField t1) (PhoneField t2) = t1 == t2
  eq _ _ = false

data ValidationError = ValidationError (Array String) Field

type Errors = Array ValidationError

initialState :: AppState
initialState = { person: examplePerson, errors: [] }

renderString :: Maybe String -> String
renderString Nothing = ""
renderString (Just string) = string

type AddresBookProps
  = { person :: Person, 
      errors :: Errors,
      onUpdate :: Person -> Errors -> Field -> Effect Unit
    }

addressBookClass :: React.ReactClass AddresBookProps
addressBookClass = React.component "AddressBook" component
  where
    component this =
      pure { state: {}
           , render: render <$> React.getProps this
           }
    render 
      { 
        person: Person person@{ homeAddress: Address address }, 
        errors,
        onUpdate
      } =
      let
        formField :: String -> String -> String -> 
          (String -> Person) -> Field -> 
          (String -> Either (Array String) String) -> 
          React.ReactElement
        formField name hint value update field validator =
          DOM.div [ Props.className "form-group" ]
            [ DOM.label [ Props.className "col-sm-2 control-label" ] 
              [ DOM.text name ]
            , DOM.div [ Props.className "col-sm-3" ]
              [ 
                DOM.input [ 
                  Props._type "text"
                  , Props.className "form-control"
                  , Props.placeholder hint
                  , Props.value value
                  , Props.onChange (\e -> do
                    let text = (unsafeCoerce e).target.value
                        person' = update text
                    case validator text of
                      Left errors' -> 
                        onUpdate person' 
                          [ValidationError errors' field] field
                      Right _ -> onUpdate person' [] field
                  )
                ]
              ]
            ]

        renderPhoneNumber :: 
          PhoneNumber -> Int -> Array React.ReactElement
        renderPhoneNumber (PhoneNumber { phoneType, number }) n = 
          [
            formField (show phoneType) "XXX-XXX-XXXX" number 
              (\number' -> 
                Person $ person { 
                  phones = fromMaybe person.phones $ 
                    modifyAt n (updatePhoneNumber number') 
                      person.phones })
            (PhoneField phoneType) validatePhoneNumber',
            DOM.div [ Props.className "row" ] 
              (renderValidationErrors errors (PhoneField phoneType))
          ]

        renderValidationError ::
          ValidationError -> React.ReactElement
        renderValidationError (ValidationError err _) = 
          DOM.div [ Props.className "alert alert-danger" ]
                  (DOM.text <$> err)

        renderValidationErrors :: Array ValidationError -> Field ->
          Array React.ReactElement
        renderValidationErrors [] _ = []
        renderValidationErrors xs field = 
          map renderValidationError $
            filter (\err -> 
                     let ValidationError str field' = err in
                       field == field') xs

        updateFirstName s = Person $ person { firstName = s }
        updateLastName  s = Person $ person { lastName  = s }

        updateStreet s = 
          Person $ 
            person { homeAddress = Address $ address { street = s } }
        updateCity   s = 
          Person $ 
            person { homeAddress = Address $ address { city   = s } }
        updateState  s = 
          Person $ 
            person { homeAddress = Address $ address { state  = s } }

        updatePhoneNumber s (PhoneNumber o) = 
          PhoneNumber $ o { number = s }
      in
      DOM.div 
        [ Props.className "container" ]
        [ 
          DOM.div [ Props.className "row" ] 
            [ DOM.form [ Props.className "form-horizontal" ] $
              [ 
                DOM.h3' [ DOM.text "Basic Information" ],
                formField "First Name" "First Name" person.firstName 
                  updateFirstName FirstNameField validateFirstName',
                DOM.div [ Props.className "row" ] 
                  (renderValidationErrors errors FirstNameField), 
                formField "Last Name" "Last Name" person.lastName 
                  updateLastName LastNameField validateLastName',
                DOM.div [ Props.className "row" ] 
                  (renderValidationErrors errors LastNameField), 
                DOM.h3' [ DOM.text "Address" ],
                formField "Street" "Street" address.street 
                  updateStreet StreetField (\p -> Right ""),
                formField "City" "City" address.city 
                  updateCity CityField (\p -> Right ""),
                formField "State" "State" address.state 
                  updateState StateField validateState',
                DOM.div [ Props.className "row" ] 
                  (renderValidationErrors errors StateField), 
                DOM.h3' [ DOM.text "Contact Information" ]
              ] <> 
                (concat $ zipWith renderPhoneNumber person.phones 
                  (0 .. length person.phones))
            ]
        ]

mainClass :: React.ReactClass {}
mainClass = React.component "Main" component
  where
  component this =
    pure 
      { 
        state: initialState
        , render: render <$> React.getState this
      }
    where
    render 
      { 
        person, 
        errors
      } =
      React.createLeafElement addressBookClass
        { person,
          errors,
          onUpdate: (\person' errors' fieldName -> 
            React.modifyState 
              this (\state -> state {
                person = person', 
                errors = (filter (\err -> 
                  let (ValidationError _ field) = err 
                  in (field /= fieldName)) state.errors)
                  <> errors'
              }))
        }

main :: Effect Unit
main = void do
  log "rendering"
  doc <- window >>= document

  let node = DOM.toNonElementParentNode doc

  element <- DOM.getElementById "main" node

  let element' = unsafePartial (fromJust element)

  ReactDOM.render (React.createLeafElement mainClass {}) element'

