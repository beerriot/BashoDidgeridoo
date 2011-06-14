# What is Basho Didgeridoo?

Basho Didgeridoo is a fork of [Basho
Banjo](https://github.com/rklophaus/BashoBanjo), and is an experiment
in using Riak Pipe (instead of Riak Core directly) to create a
distributed orchestra powered by midi files. It serves as a simple
example of one way to create a distributed application with Riak Pipe.

# How Does it Work?

To understand 'Didgeridoo, you first need to understand a little about
Riak Pipe. Riak Pipe is an open-source Erlang library that allows you
to create masterless distributed applications by building atop Riak
Core to use the principles described in [Amazon's Dynamo
Paper](http://www.allthingsdistributed.com/2007/10/amazons_dynamo.html). For
more information about Riak Core, you can view [this
presentation](http://www.slideshare.net/rklophaus/masterless-distributed-computing-with-riak-core-euc-2010).
For more about Riak Pipe, [the
README](https://github.com/basho/riak_pipe/blob/master/README.org) is
a pretty good starting point.

For our purposes, there are two main things to consider when creating
an application on Riak Pipe:

1. *Inputs* are the items that you send to the system for
processing. Didgeridoo inputs consist of a command to play a note of a
certain frequency/volume/duration.

2. The *Fitting* is a pluggable module that conforms to an interface
provided by Riak Pipe that accepts incoming inputs and acts on them.
The Didgeridoo Fitting accepts the incoming command to play a note,
generates a wave file (if it doesn't already exist) and then plays the
file using aplay or afplay, depending on the platform. (Mac and Linux
are supported... Windows is not.)

With these two things implemented, Riak Pipe does the rest of the
work. It uses a consistent hash to route notes to specific VNodes
(where the fittings are evaluated), and Riak Core takes care of
assigning VNodes (a.k.a. Virtual Nodes) to physical nodes, and
re-assigning the VNodes when you add/remove physical nodes to/from the
cluster.

The last part of the equation is some code that uses an [Erlang midi
library](https://github.com/jimm/erlang-midilib) developed by Jim
Menard to parse midi files and extract notes. The code than iterates
through the notes with the correct timing, generating play commands to
send to Riak Core.

# How Do I Run Basho Didgeridoo?

### Get the Code and Compile

    git clone https://github.com/beerriot/BashoDidgeridoo.git
    cd Didgeridoo
    make rel

### Start Basho Didgeridoo

    cd rel/basho_didgeridoo
    bin/didgeridoo console

### Play a Note (from the Erlang console)
     
    play(78).

### Play a Midi Tune

    play("../../midi/mario.mid").
    play("../../midi/zelda.mid").

### Connect to Another Didgeridoo Node

If you want to run multiple nodes on a single computer, you can:

1. Copy the files in `rel/basho_didgeridoo` to another directory.
2. Edit `etc/vm.args` and change the node name to something like `didgeridoo1@127.0.0.`
3. Edit `etc/app.config` and change the handoff port to avoid a port conflict.
4. Start the new node.
5. Join the new node to the existing node from the Erlang console: `join('didgeridoo1@hostname').`
6. Then, play a midi tune, which should play across both nodes: `play("../../midi/mario.mid").`