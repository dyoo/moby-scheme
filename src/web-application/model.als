-- A small model of WeScheme.

-- TODO:
--
-- Add more interesting operations
--
-- Rate a program.
--
-- Hide a comment.
--
-- Blacklist a user.
--
-- Moderate an application.





open util/ordering[State] as S



-- A message represents some string in the system.
sig Message {}


-- A User can invoke actions on the system, and own data.
sig User {}

-- An Administrator has superuser privileges.
sig Admin extends User {}

-- An anonymous user
sig AnonymousUser extends User {}


-- A Source is some source code written by a user.
sig Source {
    sourceUser : User
}


-- A Binary is a compilation of some source.
sig Binary {
    binarySource: Source
}


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
   admins: set users,
   moderated: set users,             -- moderated is the set of users
                                                    -- that are suspect.
   sources: set Source,
   comments: set Comment,
   visibleComments: set comments,
   binaries: set Binary
}






----------------------------------------------------------------------

-- Every action is initiated by a user, and is a transition from state to state'.
abstract sig Action {
    user: User,
    state: one State,
    state': one State
} {
    user in state.users
    permit[state, this]
}


-- Add a new normal user
sig AddUser extends Action {
    userToAdd : User
} {
      userToAdd not in state.users
 
      state'.users = state.users + userToAdd
      state'.admins = state.admins
      state'.moderated = state.moderated
      state.sources = state'.sources
      state.comments = state'.comments
      state'.visibleComments = state.visibleComments
      state.binaries = state'.binaries
}

sig AssignAsAdmin extends Action {
    superUser: User
} {
    superUser not in state.admins
    -- If the one being made a superUser is moderated, they're not trustworthy enough to be an
    -- administrator.
    superUser not in state.moderated

    state'.users = state.users
    state'.admins = state.admins + superUser
    state'.moderated = state.moderated
    state.sources = state'.sources
    state.comments = state'.comments
    state'.visibleComments = state.visibleComments
    state.binaries = state'.binaries
}


-- Changes a user so that he or she is now moderated.
sig AssignAsModerated extends Action {
    moderatedUser : User
} {
    moderatedUser not in state.admins
    moderatedUser not in state.moderated

    state'.users = state.users
    state'.admins = state.admins
    state'.moderated = state.moderated + moderatedUser
    state.sources = state'.sources
    state.comments = state'.comments
    let hiddenComments = state.comments & moderatedUser[commentUser] {
        state'.visibleComments = state.visibleComments - hiddenComments }
    state.binaries = state'.binaries
}



-- Add a new source.
sig AddSource extends Action {
  source: Source
} {
      source not in state.sources
      source.sourceUser in state.users

      state'.sources = state.sources + source
      state.users = state'.users
      state.admins = state'.admins
      state'.moderated = state.moderated
      state.comments = state'.comments
      state'.visibleComments = state.visibleComments
      state.binaries = state'.binaries
}




-- Add a new comment to an existing source.
sig AddComment extends Action {
    source: Source,
    message: Message
} {
    source in state.sources

    some comment: Comment {
        comment.commentUser = user
        comment.commentSource = source
        comment.commentMessage = message

        state.sources = state'.sources
        state.users = state'.users
        state.admins = state'.admins
        state'.moderated = state.moderated
        state'.comments = state.comments + comment
        user in state.moderated implies { state'.visibleComments = state.visibleComments }
        user not in state.moderated implies {state'.visibleComments = state.visibleComments + comment }
        state.binaries = state'.binaries        
    }
}


sig CompileSource extends Action {
    compiledSource: Source,
    resultBinary: Binary
} {
    compiledSource in state.sources
    resultBinary in Binary - state.binaries

    state.sources = state'.sources
    state.users = state'.users
    state.admins = state'.admins
    state'.moderated = state.moderated
    state'.comments = state.comments
    state'.visibleComments = state.visibleComments
    state'.binaries = state.binaries + resultBinary
}



-----------------------------------------------------------------------------------


pred init(s : State) {
    one Admin
    s.users = Admin
    s.admins = Admin
    no s.sources
    no s.comments
    no s.binaries
    no s.moderated
}


-- Permission model.
pred permit(s : State, a: Action) {
    a in AddUser implies {a.user in s.admins }

    a in AssignAsAdmin implies { a.user in s.admins }

    a in AssignAsModerated implies { a.user in s.admins }

    a in AddSource implies { a.user not in AnonymousUser and
                             (a.user in s.admins or a.user = (a <: AddSource).source.sourceUser)
                                         }

    a in AddComment implies { a.user not in AnonymousUser }

    a in CompileSource implies { a.user not in AnonymousUser and
                                 (a.user in s.admins or a.user = a.resultBinary.binarySource.sourceUser) }
}


----------------------------------------------------------------------
-- Tracing
----------------------------------------------------------------------

-- TraceActions forces a trace along a legal action sequence.
pred TraceActions {
    init[S/first[]]
    all s: State - S/last[] | let s' = S/next [s] |
    // All states are involved in an action.
    some a: Action {
      a.state = s
      a.state' = s'
    }
    // All actions pair up the states (except the last).
    State - S/last[] = Action.state

}


--- See that we can create a model where there are real non-admin users.
pred exerciseNonAdmins {
    TraceActions[]
    all s: State - S/first[] | some s.users - Admin
}


-- See that non-admins can add sources
pred exerciseAddSourceByNonAdmin {
    TraceActions[]
    AddSource in Action
    no (AddSource.user & Admin)
}

-- See that non-admins can add sources
pred exerciseCompileSourceByNonAdmin {
    TraceActions[]
    CompileSource in Action
    no (CompileSource.user & Admin)
}


pred exerciseAddCommentsByNonAdmin {
    TraceActions[]
    AddComment in Action
    no (AddComment.user & Admin)
}


-- Make sure moderation still means other people can post.
pred someoneIsModeratedButSomeoneElseIsVisible {
    TraceActions[]
    AssignAsModerated in Action
   some s:State { some s.moderated and some s.visibleComments }
}


-- We make sure all Actions are represented.  As more actions are defined,
-- add them here.
pred exhaustiveTraceActions {
    TraceActions[]
    some AddUser
    some AddSource
    some AddComment
}




------------------------------------------------------------------------------
-- Tests, assersions and predicates
-------------------------------------------------------------------------------

-- Make sure that the only comments that get in are those annotating
-- an existing source.
assert commentsOnlyOnStateSources {
   TraceActions[] implies {
     all s : State {
         s.comments.commentSource in s.sources
     }
   }
}



-- Make sure that source refer to existing users.
assert sourceUsersAreInStates {
   TraceActions[] implies {
     all s : State {
         s.sources.sourceUser in s.users
     }
   }
}


-- Make sure privileges can't be raised except by using AssignAsAdmin
assert regularUsersCannotMagicallyBecomeAdmins {
    TraceActions[] implies {
        all s : State,  u: s.users {
            u not in s.admins implies {all s': S/nexts[s] | u in s'.admins implies some a: AssignAsAdmin | a.superUser = u }
        }
    }
}



-- Sources never get removed.
assert sourcesNeverGetRemoved {
    TraceActions[] implies {
        all s: State, s':S/nexts[s] { s.sources in s'.sources }
    }
}


assert adminsCantBeModerated {
    TraceActions[] implies {
        no s: State {some (s.moderated & s.admins)}
    }
}


--- Check that moderated users don't have their comments shown.
assert moderatedUsersAreInvisible {
    TraceActions[] implies {
         all s: State, u: s.moderated {
             u not in s.visibleComments.commentUser
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
run exhaustiveTraceActions for 4
run exerciseAddSourceByNonAdmin
run exerciseCompileSourceByNonAdmin
run exerciseAddCommentsByNonAdmin
run exerciseNonAdmins
run someoneIsModeratedButSomeoneElseIsVisible for 5

check commentsOnlyOnStateSources for 5
check sourceUsersAreInStates for 5
check regularUsersCannotMagicallyBecomeAdmins for 5
check sourcesNeverGetRemoved for 5
check adminsCantBeModerated for 5
check moderatedUsersAreInvisible for 5
