# scholar 0.2.3

+ `format_publications` to format publication list (2022-06-21, Tue)
    - <https://github.com/jkeirstead/scholar/issues/110>
+ update journal ranking data
+ remove `get_impactfactor`
+ fixed when some years contain 0 cites (@jefferis, #101)
+ update documents (@jefferis, #100)

# scholar 0.2.2

+ return NULL if fail to access data (follow CRAN policy)
+ restore `pubid` (@conig, #97)
+ extending `get_profile(id)` to include professional interests (@TS404, #95)
+ allow `n_deep = 0`, for immediate author network (#timonelmer, #90)

# scholar 0.2.0

+ `set_scholar_mirror` to allow user to set a google scholar mirror (2021-01-04)
