open util/ordering[State] as S

-- A small model of WeScheme.

sig User {}
sig Source {}

sig Binary {}


-- A state consists of a set of users, a set of sources, comments associated to those
-- sources, and binaries.
sig State {
   users: set User,
   sources: users -> Source,
   comments: set Comment,
   binaries: sources -> lone Binary,
}


fact ThingsNeedToBeOwnedBySomeState {
    all u: User | some users.u
    all s: Source | some sources.s
    all c: Comment | some comments.c
    all b: Binary | some binaries.b
}


fact BinaryToOneSourcePerState{
  all s:State, b: Binary | lone s.binaries.b
  all b : Binary | some binaries.b
}



sig Comment {
    commentUser : User,
    commentSource : Source,
    commentMessage : Message
}

sig Message {}






----------------------------------------------------------------------


abstract sig Action {
    user: User,
    state: one State,
    state': one State
}

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


sig AddSource extends Action {
  source: Source
} {
     -- fixme: add check for either ownership or admin permission
      (user->source) not in state.sources
      state'.sources = state.sources + (user->source)
      state.users = state'.users
      state.comments = state'.comments
      state.binaries = state'.binaries
}


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



-- Make sure that the only comments that get in are those annotating
-- an existing source.
assert commentsOnlyOnStateSources {
   ExhaustiveTraceActions[] implies {
     all s : State {
         s.comments.commentSource = s.sources[s.users]
     }
   }
}


-- Reminder: bump up the scope as we add more Actions.
run ExhaustiveTraceActions for 4


check commentsOnlyOnStateSources
