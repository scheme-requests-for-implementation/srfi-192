Reference implementation of srfi-181 (Custom Ports) and 
srfi-192 (Port positioning).

This is not intended to be a drop-in library, since it is not 
possible to extend implementation's built-in ports portably.
Rather, it intends to illustrate details.

Portabl R7RS implementation includes an auxiliary library,
(srfi 181 adapter), which replaces I/O procedures in (scheme base),
(scheme read) and (scheme write) so that they can accept custom ports.

To run tests, load test.scm.

Reference implementation of Gauche is also provided, using its
own custom port layer (gauche.vport).  However, Gauche's vport lacks 
some functionality to fully support srfi-181.  One test fails on Gauche.

