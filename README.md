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

| Module | Description |
|--------|-------------|
| Main.main | this is calling our two "endpoint" functions, representing an incoming HTTP request
| Backend.Api | represents functions for a servant endpoints
| Backend.Api.throwAppError | "throws" a servant error, resulting in a non-success HTTP status code. In the case of this example, it just prints the error to show us what was returned to the "client".
| Backend.Domain | functions handling core business logic. nearly one-to-one with api layer functions, but these have no concept of the API or the existence of the public errors.
| Backend.Errors | Backend error types. The root error type defines a method of converting its subtypes to a public error (via the instance of `SE.ToAppError`), and all subtypes define translations the corresponding public error type. Notice some sensative backend errors are converted to non-sensative varients, like `UnhandledSQLError Text` is converted to `SE.QueryFailure`.
| Shared.Errors | This module represents an external common package shared between the backend and the frontend, allowing models, errors and routes to be used in the frontend and backend, providing compiletime garantees that the frontend/backend work together. Errors here are public, and information within them is visible to users and potential bad actors. Sensative information should never be contained or represented in these errors. This includes but is not limited to: user account status (disabled, missing, locked), raw SQL errors, raw shell command errors/output


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
