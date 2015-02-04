----------------------
# Paper Outline
- Introduction
  - strong consistency is expensive
    - go to NoSQL for the flexibility and scaling
    - use eventual consistency and hope it works out
  - usually it does! why?
      - because the operations actually don't conflict
      - people will tolerate some inconsistencies/inaccuracy 
  - benefits ADTs
      - system understands commutativity, which means it doesn't have to abort
      - express desired behavior/semantics (may be approx)
- Commutativity
  - commutativity specification
      - ex: set
  - avoid aborting txns: transactional boosting, evaluated in Retwis case study
  - phase reconciliation: another use case, not evaluated but in the pipeline
- Implementation
  - protocol: `prepare` and `commit` for each op
  - OCC, but could be locking
- Evaluation / Case Study
  - *maybe pitch this as a case study? it's certainly not going to feel like much of an "evaluation"*
  - explain Retwis application, txns:
    - `new_user:5%`, `follow:10%`, `unfollow:5%`, `post:30%`, `read_timeline:50%`
    - uniform vs powerlaw graph
    - uniform vs zipfian workload generation
  - evaluate with commutativity "on" and "off"
  - *evaluate with multiple levels of commutativity? something like comm. lattice eval?*
  - *evaluate against non-transactional?*
    - could just run the same thing without transactions or with no aborting txns as a proxy
    - but should we quantify the number of resulting conflicts/inconsistencies somehow?
  - *evaluate against eventual consistency somehow?*
- Future work
  - other potential uses of ADTs and commutativity
    - batching and combining
    - contention/hot-spot avoidance via splitting (phase reconciliation)
    - replication/fault tolerance

## Questions:
- where to work in contention / naturally skewed workload? use as motivation? or just in case study?
- where to introduce CRDTs? for this audience in particular, should have this be prominent

## Todo:
- **Implementation/Evaluation**
  - powerlaw/zipfian workload
  - try with locks rather than just OCC?
