This is a sample implementation of SRFI 181 (Custom Ports) and
SRFI 192 (Port positioning) provided by Shiro Kawai.

This is not intended to be a drop-in library, since it is not possible
to extend an implementation's built-in ports portably.  Rather, it is
intended to illustrate details.

The portable R7RS implementation includes an auxiliary library, `(srfi
181 adapter)`, which replaces I/O procedures in `(scheme base)`,
`(scheme read)` and `(scheme write)` so that they can accept custom
ports.  That is, you can say as follows to use a custom port as if it
were another kind of port:

    (import (except (scheme base)
                    input-port? output-port? textual-port? binary-port?
                    port? close-port close-input-port close-output-port
                    read-char peek-char read-line char-ready?
                    read-string read-u8 peek-u8 u8-ready?
                    read-bytevector read-bytevector!
                    write-char write-string write-u8
                    write-bytevector flush-output-port
                    file-error?)
            (srfi 181 adapter)   ;; includes read and write
            (srfi 181))

To run tests, load test.scm.

    Gauche:    gosh -I. ./test.scm   (fails 1 test, due to Gauche's vport feature)
    Chibi:     chibi-scheme -I. ./test.scm

A sample implementation for Gauche is also provided, using its own
custom port layer `(gauche.vport)`.  However, Gauche's `vport` lacks
some functionality to fully support SRFI 181.


Note on `peek-char`/`peek-u8`
-------------------------

Implementers need to be aware that `peek-char` / `peek-u8` requires buffering
in the custom port, so the port-position (where the next read or write
operation on the port occurs) and what `get-position` callback returns (where
the next `read!` callback reads from, or next `write!` callback writes to)
may differ.  The test code includes testing such subtleties.

The reference implementation adopts peek opertaions by (1) calling `get-position`
callback to remember the current underlying position, (2) calling `read!`
callback to fetch the data and buffer it, then (3) returning the fetched
data.  When `port-position` is called then, it returns the cached
position, instead of delegating it to `get-position` callback.

There may be different ways to implement it.  Just keep in mind that
letting `port-position` always call `get-position` won't work.



Note on transcoded port implementation
--------------------------------------

The sample implementation of transcoded ports assumes internal
(native) encoding is ASCII.  This allows any Scheme implementation to
try it out, but certain code paths are not tested (e.g. no character
can raise `i/o-encoding-error`).  You can still use tests, but you may
want to enhance it to test transcoding characters out of ASCII range.
