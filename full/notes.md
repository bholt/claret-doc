# Rubis experiments
- even bid-heavy workload (`{ 0.5, 2, 1, 1, 86, 4.5, 3, 2}`) produces insufficient contention, even though there is a lot of bidding
~~~yaml
UserSummary_count:   2377
NewBid_count:      102790
AddUser_count:        556
ViewAuction_count: 109895
CloseAuction_count:  1227
BrowseItems_count:   5320
OpenAuction_count:   1139
NewComment_count:    2403
## i01,i18,i72,i73 | co | nclients=4
{
  "OpenAuction": { "CategorySale": 137, "RegionSale": 87 },
  "NewBid": { "ItemBids": 131 },
  "CloseAuction": { "ItemBids": 54, "CategorySale": 190, "RegionSale": 76 },
  "ViewAuction": { "ItemBids": 286 },
  "BrowseItems": { "CategorySale": 1010, "RegionSale": 154 }
}
## i01,i18,i72,i73 | rw | nclients=4
{
  "OpenAuction": { "CategorySale": 292, "RegionSale": 107 },
  "NewBid": { "ItemBids": 1972, "UserBids": 41 },
  "CloseAuction": { "ItemBids": 35, "CategorySale": 264, "RegionSale": 133 },
  "ViewAuction": { "ItemBids": 441 },
  "BrowseItems": { "CategorySale": 976, "RegionSale": 205 },
  "NewComment": { "Rating": 18 }
}
~~~

- add closing time when opening auction
- keep index of soon-to-close auctions
- bidders prefer auctions that will close soon
- `CloseAuction` can do `NearestByScore` to `now()` to figure out which to close, but how to ensure we don't get a bunch of unwanted contention here?
  - also, if `NewBid` is selecting based on `NearestByScore` this seems like it will introduce more contention on that global list
  - we need some way to keep track of when auctions are closing that won't impact our actual measurements (presumably this behavior isn't something we wish to capture)
- *alternatively:* can we try starting by measuring the current behavior? save times of bids and time of closing and plot the distributions
  - maybe just by having them in order by id in the listings they're getting a similar behavior (but diluted over all the different listings)

```
> sql("select cca, txn_count, txn_retries, throughput from d where rate = 100 and nthreads = 32")
  cca txn_count txn_retries throughput
1  co    890612         396   14832.01
2  co    855176         408   14240.26
3  co    874420         420   14562.31
4  rw    743744       31808   12384.53
5  rw    757024       32604   12606.69
6  rw    759640       30724   12650.55
> pp.txn_conflicts(d[4,])
## i01,i18 | rw
(BrowseItems,CloseAuction,NewComment): 4
(BrowseItems,CloseAuction,ViewAuction): 3
(BrowseItems,CloseAuction): 278
(BrowseItems,OpenAuction,NewComment): 4
(BrowseItems,OpenAuction,ViewAuction,NewComment): 5
(BrowseItems,OpenAuction,ViewAuction): 4
(BrowseItems,OpenAuction): 492
(BrowseItems): 14
(CloseAuction,BrowseItems,NewComment): 1
(CloseAuction,BrowseItems,ViewAuction): 1
(CloseAuction,BrowseItems): 31
(CloseAuction,CloseAuction,NewComment): 5
(CloseAuction,CloseAuction,ViewAuction): 2
(CloseAuction,CloseAuction): 446
(CloseAuction,NewBid): 350
(CloseAuction,OpenAuction): 95
(CloseAuction): 23
(NewBid,CloseAuction): 318
(NewBid,NewBid): 235062
(NewBid,ViewAuction): 197
(NewBid): 120
(OpenAuction,BrowseItems,NewComment): 1
(OpenAuction,BrowseItems,ViewAuction): 1
(OpenAuction,BrowseItems): 40
(OpenAuction,CloseAuction,ViewAuction): 1
(OpenAuction,CloseAuction): 67
(OpenAuction,OpenAuction,NewComment): 1
(OpenAuction,OpenAuction): 146
(OpenAuction): 6
(ViewAuction,NewBid): 369
> 
## i01,i18 | co
(BrowseItems,CloseAuction,NewComment): 2
(BrowseItems,CloseAuction,ViewAuction): 4
(BrowseItems,CloseAuction): 388
(BrowseItems,OpenAuction,NewComment): 37
(BrowseItems,OpenAuction,ViewAuction): 16
(BrowseItems,OpenAuction): 735
(BrowseItems): 74
(CloseAuction,BrowseItems,NewComment): 3
(CloseAuction,BrowseItems,ViewAuction): 3
(CloseAuction,BrowseItems): 35
(CloseAuction,CloseAuction): 230
(CloseAuction,NewBid): 195
(CloseAuction,OpenAuction,NewComment): 9
(CloseAuction,OpenAuction,ViewAuction): 1
(CloseAuction,OpenAuction): 174
(CloseAuction): 80
(NewBid,CloseAuction): 236
(NewBid,ViewAuction): 250
(NewBid): 63
(NewComment,UserSummary): 25
(OpenAuction,BrowseItems,ViewAuction): 2
(OpenAuction,BrowseItems): 52
(OpenAuction,CloseAuction,NewComment): 6
(OpenAuction,CloseAuction): 85
(OpenAuction): 10
(ViewAuction,NewBid): 571
(ViewAuction): 88
> 
## i01,i18 | rw
{
  "OpenAuction": {
    "CategorySale": 175,
    "RegionSale": 88
  },
  "NewBid": {
    "ItemBids": 185238,
    "UserBids": 50459
  },
  "CloseAuction": {
    "ItemBids": 350,
    "AuctionClosed": 167,
    "CategorySale": 245,
    "RegionSale": 192
  },
  "ViewAuction": {
    "ItemBids": 369
  },
  "BrowseItems": {
    "CategorySale": 605,
    "RegionSale": 199
  }
}
## i01,i18 | co
{
  "OpenAuction": {
    "CategorySale": 116,
    "RegionSale": 39
  },
  "NewBid": {
    "ItemBids": 549
  },
  "CloseAuction": {
    "ItemBids": 234,
    "AuctionClosed": 260,
    "CategorySale": 142,
    "RegionSale": 94
  },
  "ViewAuction": {
    "ItemBids": 659
  },
  "BrowseItems": {
    "CategorySale": 960,
    "RegionSale": 296
  },
  "NewComment": {
    "Rating": 25
  }
}
```
