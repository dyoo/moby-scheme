open util/ordering[State] as S

-- A small model of WeScheme.

-- A message represents some string in the system.
sig Message {}


-- A User can invoke actions on the system, and own data.
sig User {}

-- A Source is some source code written by a user.
sig Source {
    sourceUser : User
}


-- A Binary is a compilation of some source.
sig Binary {}


-- A comment is associated to the user making the comment.
sig Comment {
    commentUser : User,
    commentSource : Source,
    commentMessage : Message
}


-- A state consists of a set of users, a set of sources, comments associated to those
-- sources, and binaries.
sig State {
   users: set User,
   sources: set Source,
   comments: set Comment,
   binaries: sources -> lone Binary,
}






----------------------------------------------------------------------

-- Every action is initiated by a user, and is a transition from state to state'.
abstract sig Action {
    user: User,
    state: one State,
    state': one State
}


-- Add a new user
sig AddUser extends Action {
    userToAdd : User
} {
      -- fixme: add check for admin permission
      userToAdd not in state.users 
      state'.users = state.users + userToAdd
      state.sources = state'.sources
      state.comments = state'.comments
      state.binaries = state'.binaries
}


-- Add a new source.
sig AddSource extends Action {
  source: Source
} {
     -- fixme: add check for either ownership or admin permission
      source not in state.sources
      source.sourceUser in state.users

      state'.sources = state.sources + source
      state.users = state'.users
      state.comments = state'.comments
      state.binaries = state'.binaries
}




-- Add a new comment to an existing source.
sig AddComment extends Action {
    source: Source,
    message: Message
} {
    some comment: Comment {
        comment.commentUser = user
        comment.commentSource = source
        comment.commentMessage = message

        state.sources = state'.sources
        state.users = state'.users
        state'.comments = state'.comments + comment
        state.binaries = state'.binaries        
    }
}


--sig CompileSource extends Action {
  --u: User,
  --s: Source
--}


pred init(s : State) {
    no s.users
    no s.sources
    no s.comments
    no s.binaries
}



----------------------------------------------------------------------
-- Tracing
----------------------------------------------------------------------

-- TraceActions forces a trace along a legal action sequence.
pred TraceActions {
    init[S/first[]]
    all s: State - last[] | let s' = S/next [s] |
    some a: Action {
      a.state = s
      a.state' = s'
    }

}


-- We make sure all Actions are represented.  As more actions are defined,
-- add them here.
pred ExhaustiveTraceActions {
    TraceActions[]
    some AddUser
    some AddSource
    some AddComment
}




------------------------------------------------------------------------------
-- Test Assersions
-------------------------------------------------------------------------------

-- Make sure that the only comments that get in are those annotating
-- an existing source.
assert commentsOnlyOnStateSources {
   ExhaustiveTraceActions[] implies {
     all s : State {
         s.comments.commentSource = s.sources
     }
   }
}


/* We want to ensure that a binary can't be shared by multiple
sources.  */
assert binaryToOneSourcePerState{
   ExhaustiveTraceActions[] implies {
      all s:State, b: Binary | lone s.binaries.b
   }
}



-- Make sure that source refer to existing users.
assert sourceUsersAreInStates {
   ExhaustiveTraceActions[] implies {
     all s : State {
         s.sources.sourceUser = s.users
     }
   }
}


/* Just so we don't see extraneous objects in the model.  Not
essential.*/

pred ThingsNeedToBeOwnedBySomeState {
    all u: User | some users.u
    all s: Source | some sources.s
    all c: Comment | some comments.c
    all b: Binary | some binaries.b
}




-- Reminder: bump up the scope as we add more Actions.
run ExhaustiveTraceActions for 4


check commentsOnlyOnStateSources
check sourceUsersAreInStates
check binaryToOneSourcePerState