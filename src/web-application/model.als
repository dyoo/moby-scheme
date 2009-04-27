-- A small model of WeScheme.

sig User {}
sig Source {}
sig Comment {}
sig Binary {}


one sig State {
   users: set User,
   sources: users -> Source,
   comments: sources -> Comment,
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

fact CommentsToOneSourcePerState {
  all s:State, c: Comment | lone s.comments.c
  all c: Comment | some comments.c
}





----------------------------------------------------------------------


abstract sig Action {}

sig AddUser extends Action {
  u : User
}

sig AddSource extends Action {
  u: User,
  s: Source
}

sig AddComment extends Action {
  u: User,
  s: Source,
  c: Comment
}

sig CompileSource extends Action {
  u: User,
  s: Source
}


----------------------------------------------------------------------


run {}
