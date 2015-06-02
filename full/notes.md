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