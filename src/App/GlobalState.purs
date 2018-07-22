module App.GlobalState
( tasks
) where

import Prelude
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import App.Tasks.Models (Task)


tasks :: Ref (Array Task)
tasks = unsafePerformEffect $ Ref.new []
