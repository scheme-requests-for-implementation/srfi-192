This is a sample implementation of srfi-181 (Custom Ports) and
srfi-192 (Port positioning) provided by Shiro Kawai.

This is not intended to be a drop-in library, since it is not possible
to extend implementation's built-in ports portably.  Rather, it
intends to illustrate details.

Portable R7RS implementation includes an auxiliary library, (srfi 181
adapter), which replaces I/O procedures in (scheme base), (scheme
read) and (scheme write) so that they can accept custom ports.

To run tests, load test.scm.

   Gauche:    gosh -I. ./test.scm
   Chibi:     chibi-scheme -I. ./test.scm

Reference implementation of Gauche is also provided, using its own
custom port layer (gauche.vport).  However, Gauche's vport lacks some
functionality to fully support srfi-181.

Note on peek-char / peek-u8
---------------------------

Draft #5 omitted peek-char/u8 to avoid complications of port position
tracking.  However, one-character look-ahead is needed to implement 'read'
and 'read-line', so at least the custom textual port needs to have some
sort of peek functionality internally, even if it doesn't export the feature.

For now, I keep them in the library.
