-module(gleam_manifold_ffi).
-export([unwrap_subject/1]).

unwrap_subject({subject, Pid, Ref}) ->
    {ok, {Pid, Ref}};
unwrap_subject(_Subject) ->
    {error, nil}.
