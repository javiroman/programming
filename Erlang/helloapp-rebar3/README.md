helloapp
=====

An OTP application created with

    $ rebar3 new app helloapp

Build
-----

    $ rebar3 compile

Shell
-----

    $ rebar3 shell

1> application:start(helloapp).
Hello, World!
ok
2> application:stop(helloapp). 
ok
3> 
=INFO REPORT==== 17-Aug-2018::12:08:56 ===
    application: helloapp
    exited: stopped
    type: temporary

