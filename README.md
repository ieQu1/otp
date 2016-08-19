Erlang/OTP
==========

This experimental fork adds proof-of-concept **jails** to the BEAM
VM. The new "security policy" is quite primitive:

A jailed process can only send messages to processes in the same jail
and to the parent process (which is the only communication point with
the outer world). Other processes are invisible for it (but a jailed
process still can receive messages from the "free" processes).

For the sake of simplicity jails can't be nested. Distributed erlang
features probably won't work properly.

It also would be cool to have lists of forbidden functions and limit
the number of redutions and allocations per jail.

New BIFs:

*  create_jail/1 :: (Options :: list()) -> jail_id().

*  enumerate_jail_procs/2 :: (fun(), jail_id()) -> ok.

*  spawn_jail/4 :: (atom(), atom(), list(), jail_id()) -> pid().

   [1]: http://www.erlang.org
   [2]: http://wiki.github.com/erlang/otp/contribution-guidelines
   [3]: http://www.erlang.org/static/doc/mailinglist.html
   [4]: http://erlang.github.com/otp/
   [5]: HOWTO/INSTALL.md
   [6]: https://github.com/erlang/otp/wiki/Bug-reports
   [7]: http://bugs.erlang.org
