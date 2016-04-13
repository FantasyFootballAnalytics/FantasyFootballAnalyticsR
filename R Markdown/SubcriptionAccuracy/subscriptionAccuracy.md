Introduction {#introduction}
------------

In this article we examine wheter there are advantages to subsribing to
projections for fantasy football. Specifically we look at whether data
from subscription sources have higher accuracy. There are arguments for
subscription sources to be more accurate as you may expect to get better
accuracy as part of what you are paying for. We examine data from the
2015 season for QB, RB, WR and TE positions.

Overall accuracy {#overall-accuracy}
----------------

We take the projected seasonal points and compare with the acutal
points. We have 10 free sources and 6 subscription sources. For each of
the groups we calculate *R<sup>2</sup>* and MASE values. The results are
below.

| Source Type  |  R-Squared|  MASE|
|:-------------|----------:|-----:|
| Free         |       0.63|  0.56|
| Subscription |       0.62|  0.57|

Although not by much, the subscription sources are less accurate than
the free sources. Part of that could be attributed to the fact there are
more free sources than subscription sources. To investigate that further
we look at combinations of 6 sources from the free sources. The results
below show the minimum, maximum, mean and median *R<sup>2</sup>* and
MASE values for all the combinations. While there are combinations of 6
free sources that are worse than the subcription source there are also
combinations that are significantly better than the subscription
sources. Overall we cannot say that subscription sources are more
accurate than the free sources.

| Measure |  R-Squared|  MASE|
|:--------|----------:|-----:|
| Min     |       0.60|  0.55|
| Max     |       0.64|  0.59|
| Mean    |       0.63|  0.57|
| Median  |       0.63|  0.57|

Position Accuracy {#position-accuracy}
-----------------

Let's take a look and see if the results are different when we look at
individual positions:

| Source Type  | Position |  R-Squared|  MASE|
|:-------------|:---------|----------:|-----:|
| Free         | QB       |       0.72|  0.41|
| Subscription | QB       |       0.69|  0.43|
| Free         | RB       |       0.49|  0.67|
| Subscription | RB       |       0.48|  0.69|
| Free         | WR       |       0.62|  0.60|
| Subscription | WR       |       0.57|  0.63|
| Free         | TE       |       0.58|  0.56|
| Subscription | TE       |       0.58|  0.56|

Except for the TE position, free sources are also more accurate when we
look at positions separately. The difference is greatest with the WR
positions, RB accuracy is closer.

Conclusion {#conclusion}
----------

We have seen that subscription sources are not more accurate than the
free sources, so the if you are paying for a fantasy football
subscription just to get the projections you may not be worse off just
by using the available free sources such as NFL, Yahoo, CBS, ESPN.
Another interesting result from this analysis is that it does matter
which combination of sources you use. As we saw when we look at
combinations of free sources there is a difference in accuracy when you
look at the same number of sources. The accuracy of all free sources
seemed to be around the mean/median of any combincation of 6 free
sources. We will later analyze the effect on accuracy of adding sources
to the projections.
