-module(gleam_manifold_ffi).
-export(['receive'/1, 'receive'/2]).

'receive'({subject, Ref}) ->
    receive
        {Ref, Message} -> Message
    end.

'receive'({subject, Ref}, Timeout) ->
    receive
        {Ref, Message} -> {ok, Message}
    after Timeout ->
        {error, nil}
    end.
