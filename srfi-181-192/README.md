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
                    write-bytevector flush-output-port)
            (srfi 181 adapter)   ;; includes read and write
            (srfi 181))

To run tests, load test.scm.

    Gauche:    gosh -I. ./test.scm   (fails 1 test, due to Gauche's vport feature)
    Chibi:     chibi-scheme -I. ./test.scm

A sample implementation for Gauche is also provided, using its own
custom port layer `(gauche.vport)`.  However, Gauche's `vport` lacks
some functionality to fully support SRFI 181.

Note on `peek-char` / `peek-u8`
---------------------------

Draft #5 omitted `peek-char`/`u8` to avoid complications of port position
tracking.  However, one-character look-ahead is needed to implement `read`
and `read-line`, so at least the custom textual port needs to have some
sort of peek functionality internally, even if it doesn't export the feature.

For now, I keep them in the library.


Note on transcoded port implementation
--------------------------------------

The sample implementation of transcoded ports assumes internal
(native) encoding is ASCII.  This allows any Scheme implementation to
try it out, but certain code paths are not tested (e.g. no character
can raise `i/o-encoding-error`).  You can still use tests, but you may
want to enhance it to test transcoding characters out of ASCII range.
