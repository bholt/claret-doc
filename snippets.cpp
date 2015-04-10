template< class T >
class Set {
  Interval<int> size(float error); // [ value, value+size()*error )
  Probabilistic<int> size(float error); // hyperloglog
  Probabilistic<bool> contains(T o); // bloom filter
  Inconsistent<T> random();
};

template< class T >
class SetCRDT {
  Inconsistent<int>  size(float error);
  Inconsistent<bool> contains(T o);
  Inconsistent<T>    random();
};

SetCRDT<string> retweeters = /*...*/;
Inconsistent<int> retweet_count = retweeters.size();

transaction<linearizable> {
  if (retweet_count == 0) { // <- disallowed
    send_message("You don't have any friends.");
  }
}
