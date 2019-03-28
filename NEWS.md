# v0.1.2.8000
- Fixing bug in test scripts introduced by
  `RCurl::url.exists()`. Regular words do suddenly return `TRUE`. WTF?
  But entering a invalid URL does still result in `FALSE`.
# v0.1.1
- Fixing namespacing in the example of `get.dwd.ftp.url()`.
- Explicitly use `stats` namespace for `rnorm` function in
  `dwd2r.url.check()`
- Adding the **dwd2r.md** vignette taken from the corresponding blog
  post.
- A bug in the `readline()` function causes it to only read multiple
  characters if there is no trailing whitespace. There was, however,
  one present in each call and thus all choices larger than 9 could
  not be accessed
# v0.1.0
Splitting the climex package into three different pieces. This part
will handle the scrapping, downloading, and formatting of the data
provided by the German weather service DWD
