# Evaluation
\label{sec:evaluation}

To show the efficacy of leveraging commutative operations, we use an application typical of web workloads: a simplified Twitter clone known as *Retwis*.

...

We can see in Figure \ref{fig:throughput} that leveraging concurrency results in much higher throughput. The baseline, which doesn't leverage commutativity, must abort one or the other whenever two transactions attempt to access the same record, resulting in the increasing abort rates we see in Figure \ref{fig:abort_rate}.