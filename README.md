# Public/Private Errors

## Description

This is an example of an error structure to enforce public and private error
typing, which is useful to prevent errors containing sensative information from
leaking out of an API.

## Representation

This is a contrived example, but aims to represent a more complicated
architecture. Since so much of this code is representative, the following is a
detailed description of each module and where you might find it in a larger
application.

TODO, ran out of day

## Running

Run with cabal. It was tested using GHC 7.10.3, but should work with newer
versions as well (there's nothing particularly new-age in this).

```
[vagrant@localhost perrors]$ cabal run
Preprocessing executable 'perrors' for perrors-0.1.0.0..
Building executable 'perrors' for perrors-0.1.0.0..
Running perrors...
ErrorAuth BadCredentials
ErrorDb QueryFailure
```

Here we see the final form of the errors that the application is throwing from
the domain logic. What's most important to note is that in the depths of the
domain logic, we returned `AccountDisabled` and `FailedConstraint`, but what the
"API" presented to us were safe error messages defined not in the backend, but
in the shared error module.

Better yet, we didn't have to expicitly handle this error conversion in the
domain logic (it was done on the class interface by the error definition). Plus,
we didn't have to spend much energy dealing with converting our `BackendError`
to the public errors- it's all packaged in a single case statement in the API
error handling layer, and constrained to prevent unsafe errors from slipping
through.
