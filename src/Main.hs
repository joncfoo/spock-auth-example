{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- ghc seems to think that the constraints we provide below are redundant?
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Main where

-- HVect provides heterogenous lists which we use to specify constraints in the
-- type system
import Data.HVect (HVect(..), ListContains, NotInList)

import Network.HTTP.Types.Status
import Web.Spock
import Web.Spock.Config


data User
  = User
  { userName :: String
  , userRole :: Role
  }

data Role = Author | Admin


-- data we will store in the web session
type Session = Maybe User


-- data types which we will use as authorization constraints further below
data IsGuest = IsGuest
data IsAuthor = IsAuthor
data IsAdmin = IsAdmin


-- our "database"
usersDB :: [(String, (String, Role))]
usersDB =
  [ ("admin", ("qwerty", Admin))
  , ("author", ("asdfgh", Author))
  ]


main :: IO ()
main =
  do
    spockCfg <- defaultSpockCfg Nothing PCNoDatabase ()
    --    default session data  ^       ^            ^
    --     no database for our web app -/            |
    --                            application state -/
    runSpock 5000 (spock spockCfg webapp)


type App ctx = SpockCtxM ctx () Session () ()
type Action ctx a = SpockActionCtx ctx () Session () a


webapp :: App ()
webapp =
  -- apply the baseHook for all routes - this puts an empty HVect in the context
  prehook baseHook $

    do get "/" (text "anyone can access this")

       prehook guestHook $
       -- only guests are authorized to use these routes

         do get "/guest" (text "hi guest!")
            post "/sign-in" signInAction

       prehook userHook $
       -- only logged in users are authorized to use these routes

         do get "/sign-out" signOutAction
            get "/secret" secretAction

            prehook (roleHook IsAdmin) $
              -- only admin is authorized
              get "/settings" settingsAction

            prehook (roleHook IsAuthor) $
              -- only author is authorized
              get "/lounge" authorLoungeAction


{--
Actions

Actions make use of type constraints against the context to ensure that they can
only be placed [and run] in certain areas in the code where the context is
valid.

Note: try to shuffle some of the actions under the various hooks - type errors
will ensue :)

--}


signInAction
  :: ( ListContains n IsGuest xs  -- ensure that the context contains `IsGuest` data type
     , NotInList User xs ~ 'True  -- and does not contain the `User` data type
     )
  => Action (HVect xs) a
signInAction =
  do username <- param' "username"
     password <- param' "password"

     -- verify credentials
     case lookup username usersDB of
       Nothing ->
         -- no user found
         do setStatus unauthorized401
            text "who are you?"

       Just (p, role) ->
         case p == password of
           False ->
             -- invalid password
             do setStatus unauthorized401
                text "who are you?"

           True ->
             -- store the data in the session
             do sessionRegenerateId
                writeSession (Just (User username role))
                redirect "/"


signOutAction
  :: ListContains n User xs   -- ensure `User` is in the context
  => Action (HVect xs) a
signOutAction =
  do writeSession Nothing
     redirect "/"


secretAction
  :: ListContains n User xs    -- ensure `User` is in the context
  => Action (HVect xs) a
secretAction =
  text "Ah, you found me!"


settingsAction
  :: ListContains n IsAdmin xs  -- ensure `IsAdmin` is in the context
  => Action (HVect xs) a
settingsAction =
  text "admin settings go here"


authorLoungeAction
  :: ListContains n IsAuthor xs  -- ensure `IsAuthor` is in the context
  => Action (HVect xs) a
authorLoungeAction =
  text "welcome to the author's only lounge!"


{--
Hooks

These hooks modify the HVect in the context under certain conditions. Actions
that operate "under" these hooks should only be valid under these hooks unless
they are very generic (i.e. don't require a specific context).

--}

baseHook :: Action () (HVect '[])
baseHook =
  -- put an empty HVect into the context
  pure HNil


guestHook :: Action (HVect xs) (HVect (IsGuest ': xs))
guestHook =
  -- add `IsGuest` into the context if applicable
  readSession >>= \case
    Nothing ->
      do ctx <- getContext
         pure (IsGuest :&: ctx)
    Just _ ->
      redirect "/"


userHook :: Action (HVect xs) (HVect (User ': xs))
userHook =
  -- add `User` into the context if applicable
  readSession >>= \case
    Nothing ->
      do setStatus forbidden403
         text "please sign-in"
    Just u ->
      do ctx <- getContext
         pure (u :&: ctx)


roleHook :: a -> Action (HVect xs) (HVect (a ': xs))
roleHook role =
  -- add `role` to the context is applicable
  readSession >>= \case
    Nothing ->
      do setStatus forbidden403
         text "please sign-in"
    Just user ->
      case userRole user of
        Admin ->
          do ctx <- getContext
             pure (role :&: ctx)
        _ ->
          do setStatus unauthorized401
             text "nothing to see here. move along"
