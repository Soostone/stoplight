# stoplight

This package provides a simple in-memory rate limiter based on the
leaking bucket model. It currently has a single module
`Stoplight.Bucket` with the intention of adding more rate-limiting
models over time within the same `Stoplight` namespace.


## Stoplight.Bucket

A simple in-memory rate limiter based on the leaking-bucket model. You
can spend any capacity already available in the bucket. When it's
full, you can burst it down. When it's empty, you need to wait for it
to sufficiently fill up before you can continue.

Internally, it uses semaphores and a forkIO'ed background thread to
achieve its effect. This means each has a small overhead and you have
to be careful when using a large number of limiters concurrently.
