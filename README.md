# pwnchk

This tool provides a very basic API on top of Have I Been Pwned's API.  [The
full API specification is availble here](https://haveibeenpwned.com/API/v2) to
allow you to check to see if a given account and/or password has been
compromised.  Note you should not use this applicaiton.  It is untested, likely
insecure, and in no case should be relied upon for your security.

## Requirements

This tool was created to adhere to the specific requirements for a take-home
development evaluation.  The requirements as given were:

1. Create a client for this API https://haveibeenpwned.com/API/v2
   1. Terminal client, GUI, web, etc
1. Add as much functionality as you would like
1. Add some documentation on how we can use it
1. (Required) Push your project to github
1. (Required) Try not to use external open source tools/libraries other than basic HTTP clients, the less the better

Due to time constraints this application implements only a very small part of
the overall API, with a rudimentary command line interface.

## Building

This application is built using [stack](http://haskellstack.org).  To build the
application install stack as per the documentation, and then run:

```
user@host$ stack build
```

## Running

You can execute the application from within the source directory with
`stack exec`:

```
user@host$ stack exec pwnchk -- <account|password> [args]
```

The application is modal, with the first positional argument determining the
mode, and subsequent arguments being passed along to the mode.  A full list of
the modes and their arguments are available by using the `help` mode or, in any
mode, passing in the `--help` flag:

```
user@host$ stack exec pwnchk -- help
user@host$ stack exec pwnchk -- account --help
user@host$ stack exec pwnchk -- password --help
```

### The `account` mode

```
user@host$ stack exec pwnchk -- account [options] <account>
```

The account mode will display a list of breaches that an account was involved
in.  By default only the names of breaches are provided, but you may provide.

#### Options

1. `--verbose` will display additional information about the account breaches
1. `--include-unverified` will display unverified breaches in addition to
   verified breaches.

### The `password` mode

```
user@host$ stack exec pwnchk -- password [options]
```

The password mode will let you know if a given password has been compromised.
By default the application will prompt you to enter the password from the
command line.  You may pass the password in using the `--hash` or
`--unsafe-password` flag, but note that both of these modes are less secure than
the default mode of operation.

#### Options

1. `--hash=<sha1>` do not prompt for the password, instead use the password hash
   provided.  Note that while this is safer than `--unsafe-password` the
   password hash may still be vulnerable to compromise, so this option is less
   secure than the standard mode.
1. `--unsafe-password=<password>` use the provided plaintext password.  Note:
   this is unsafe for several reasons.  Do not use this option for any passwords
   that you are actively using.

## Future Work
