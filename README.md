# Experiments with SNARGs

This repo is intended to keep some my (future) experiments
with interactive proofs, in particular I'm interested into
succinct non-interactive arguments.

## Distributed database

Database with abilities of:

* Instant synchronization of changes (and conflict resolution)
* Somewhat delayed change replication augmented by lazy data querying
* Ability for nodes to keep track of some subset of keys

This should be a relational database for which creators would strive to
encode all necessary invariants into db.

Heh, maybe a SQL-like database which would allow to make blockchains
easy is what can be actually proposed to the market?

Okay, this is an aspiration, need to formulate some goal for a prototype.

Let it be key-value db with triggers.
I.e. Haskell frontend and backend with in-memory maps and/or rocksdb.

There's closed cluster of nodes.
Each node is capable of extending the DB with a transaction
(as long as triggers execute correctly).

All changes are assumed to be distributed instantly.
Every node has an id and in case of tx conflict, node's tx with
`id1 < id2` is preferred.


## Which SNARGs

Which SNARG to try implementing?
One can go with ZK-SNARKs from lib-snark as a starting point.

Or maybe something other is available?

### Bulletproofs

Started reading articles on Pedersen-commitment-based ZK proofs.
