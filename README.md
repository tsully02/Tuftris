```
███╗   ███╗██╗   ██╗██╗  ████████╗██╗██████╗ ██╗      █████╗ ██╗   ██╗███████╗██████╗ 
████╗ ████║██║   ██║██║  ╚══██╔══╝██║██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗
██╔████╔██║██║   ██║██║     ██║   ██║██████╔╝██║     ███████║ ╚████╔╝ █████╗  ██████╔╝
██║╚██╔╝██║██║   ██║██║     ██║   ██║██╔═══╝ ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗
██║ ╚═╝ ██║╚██████╔╝███████╗██║   ██║██║     ███████╗██║  ██║   ██║   ███████╗██║  ██║
╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═╝   ╚═╝╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
                  ████████╗███████╗████████╗██████╗ ██╗███████╗
                  ╚══██╔══╝██╔════╝╚══██╔══╝██╔══██╗██║██╔════╝
                     ██║   █████╗     ██║   ██████╔╝██║███████╗
                     ██║   ██╔══╝     ██║   ██╔══██╗██║╚════██║
                     ██║   ███████╗   ██║   ██║  ██║██║███████║
                     ╚═╝   ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝╚══════╝

```

Build
-----

    $ rebar3 compile

Run 
-----
  You must build before you run the following commands, and you must be running 
  it on the Halligan server. Once the server is up, you can make rooms until 
  it is down.

    Server:
    $ ssh vm-hw00
    $ cd _build/default/lib/tetris/ebin
    $ erl -name lebronjamie -setcookie monster
    1> server:start_link().

    Client:
    $ ./tetris

We have one bug with Tuftris that we have never been able to figure out. 
Sometimes, certain homework VMs aren’t reliable (we’ve had the most trouble 
with 5, 7,  and 9). When you launch the client (with ./tetris) and attempt to 
join a game (via single or multiplayer), sometimes an error will be thrown and 
the whole screen will get messed up. The reason is typically “nodedown” (it 
will be an atom in an Erlang tuple). When this happens, try sshing into a
different VM until it works. We’ve typically seen this on VMs that are 
particularly busy (often it’s the VM you get when you first log in). We were 
never able to pin down this bug, as it comes up inconsistently.
    


File Listing
------------
tetris.erl: Main file, core game logic
tetris_io.erl: Low level interfacing with cecho
tetromino.erl: Functionality for creating and manipulating tetrominos (pieces)
board.erl: Functionality for manipulating game boards
tetris.hrl: Shared header for constants
server.erl: Game master server, in change of managing rooms
game.erl: Game room server
painter.erl: Code for the painter process that is in change of tracking
             everything that needs to be drawn and drawing it.