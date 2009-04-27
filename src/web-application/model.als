-- A small model of WeScheme.

sig User {}
--sig Source {}
--sig Comment {}
--sig Binary {}


sig State {
   users: set User,
--   sources: users -> Source,
--   comments: sources -> Comment,
--   binaries: sources -> lone Binary,
}


fact ThingsNeedToBeOwnedBySomeState {
--    all u: User | some users.u
--    all s: Source | some sources.s
--    all c: Comment | some comments.c
--   all b: Binary | some binaries.b
}


--fact BinaryToOneSourcePerState{
 -- all s:State, b: Binary | lone s.binaries.b
 -- all b : Binary | some binaries.b
--}

--fact CommentsToOneSourcePerState {
 -- all s:State, c: Comment | lone s.comments.c
 -- all c: Comment | some comments.c
--}





----------------------------------------------------------------------


abstract sig Action {
}

sig AddUser extends Action {
    u: one User
} {
  all s, s': State {
      ApplyAction[s, s', this] implies {
          u not in s.users
          s'.users = s.users + u
--          s.sources = s'.sources
--          s.comments = s'.comments
--          s.binaries = s'.binaries
      }
  }
}

--sig AddSource extends Action {
 -- u: User,
 -- s: Source
--} {
 --   all state, state': State {
  --      ApplyAction[state, state', this] implies {
  --          (u->s) not in state.sources
   --         state'.sources = state.sources + (u->s)
    --        state.users = state'.users
    --        state.comments = state'.comments
     --       state.binaries = state'.binaries
      --  }
   -- }
--}

--sig AddComment extends Action {
 -- u: User,
 -- s: Source,
  --c: Comment
--}

--sig CompileSource extends Action {
  --u: User,
  --s: Source
--}


pred ApplyAction(s, s': State, a: Action) {}

----------------------------------------------------------------------


run {
    some a: AddUser, s, s': State {
        ApplyAction[s, s', a]}}
