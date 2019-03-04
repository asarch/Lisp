# Connections

## Emacs to Common Lisp Server

Just type in the buffer area:

```Emacslisp
M-x slime-connect <your-phone-ip-address>:4005
```

## Emacs to Slime server

SBCL per se cannot serve as swank, you have to do it throught Slime in an Emacs session:

```
+------+    +-------+    +-------+    +-----------------+    +-------+    +-------+
| SBCL |<---| Slime |--->| Swank |--->| ip_address:4005 |<---| Slime |<---| Emacs |
+------+    +-------+    +-------+    +-----------------+    +-------+    +-------+
```

In the Slime server REPL start the swank server:

```
(SWANK:CREATE-SERVER :PORT 4005)
```

To create a persisten server:

```
(SWANK:CREATE-SERVER :PORT 4005 :STYLE :SPAWN :DONT-CLOSE T)
```

From the Emacs client session open the SSH tunnel:

```
[$] ssh -L4005:127.0.0.1:4005 <user>@<host>
```

Now, from the Emacs client session 

```
M-x slime-connect
Host: 127.0.0.1
Port: 4005
```
