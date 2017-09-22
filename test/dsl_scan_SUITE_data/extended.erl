-module extended.

green * receiving {type, Message} ->
    send Message to nout,
    become red.

red * spontaneous {exit, Code} ->
    become blue.
